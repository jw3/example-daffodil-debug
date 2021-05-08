package ddb.debugger.dapodil

import cats.Show
import cats.effect._
import cats.effect.std.Dispatcher
import java.io.InputStream
import java.net.URI
import org.apache.daffodil.sapi.infoset.NullInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.processors.parsers._
import fs2._
import cats.effect.std.Queue

/** A running Daffodil parse. */
trait Parse {
  def events(): Stream[IO, Parse.Event]

  def step(): IO[Unit]
  def continue(): IO[Unit]
  def pause(): IO[Unit]
}

object Parse {
  def apply(
      schema: URI,
      data: InputStream,
      dispatcher: Dispatcher[IO],
      compiler: Compiler
  ): IO[Parse] =
    for {
      queue <- Queue.bounded[IO, Option[Event]](10)
      state <- State.create
      debugger = new Debugger {

        override def init(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            queue.offer(Some(Event.Init(pstate, processor)))
          }

        override def fini(processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            queue.offer(Some(Event.Fini(processor))) *> queue.offer(None) // terminate the stream with None
          }

        override def startElement(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            for {
              _ <- queue.offer(Some(Event.StartElement(pstate, processor)))
              _ <- state.await // blocks until external control says to unblock, for stepping behavior
            } yield ()
          }

        override def endElement(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            queue.offer(Some(Event.EndElement(pstate, processor)))
          }
      }
      dp <- compiler.compile(schema).map(p => p.withDebugger(debugger).withDebugging(true))
      _ <- IO
        .blocking {
          dp.parse(
            new InputSourceDataInputStream(data),
            new NullInfosetOutputter()
          )
        }
        .onError(t => IO(t.printStackTrace()))
        .start
    } yield new Parse {
      def events(): Stream[IO, Event] = Stream.fromQueueNoneTerminated(queue)

      def step(): IO[Unit] = state.step()
      def continue(): IO[Unit] = state.continue()
      def pause(): IO[Unit] = state.pause()
    }

  sealed trait Event

  // TODO: it's probably very dubious to assume PState and Parser are immutable
  object Event {
    case class Init(pstate: PState, parser: Parser) extends Event
    case class StartElement(pstate: PState, parser: Parser) extends Event
    case class EndElement(pstate: PState, parser: Parser) extends Event
    case class Fini(parser: Parser) extends Event

    implicit val show: Show[Event] = Show.fromToString
  }

  sealed trait State {
    def await(): IO[Unit]

    def continue(): IO[Unit]
    def step(): IO[Unit]
    def pause(): IO[Unit]
  }

  object State {

    sealed trait Mode
    case object Running extends Mode
    case class Stopped(whenContinued: Deferred[IO, Unit]) extends Mode

    def create(): IO[State] =
      for {
        whenContinued <- Deferred[IO, Unit]
        state <- Ref[IO].of[Mode](Stopped(whenContinued))
      } yield new State {
        def await(): IO[Unit] =
          state.get.flatMap {
            case Running                => IO.unit
            case Stopped(whenContinued) => whenContinued.get
          }

        def step(): IO[Unit] =
          for {
            nextContinue <- Deferred[IO, Unit]
            _ <- state.modify {
              case Running                => Running -> IO.unit
              case Stopped(whenContinued) => Stopped(nextContinue) -> whenContinued.complete(())
            }.flatten
          } yield ()

        def continue(): IO[Unit] =
          state.modify {
            case Running                => Running -> IO.unit
            case Stopped(whenContinued) => Running -> whenContinued.complete(()).void
          }.flatten

        def pause(): IO[Unit] =
          for {
            nextContinue <- Deferred[IO, Unit]
            _ <- state.update {
              case Running    => Stopped(nextContinue)
              case s: Stopped => s
            }
          } yield ()
      }
  }

}
