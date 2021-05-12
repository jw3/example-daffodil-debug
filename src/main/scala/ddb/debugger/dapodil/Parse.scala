package ddb.debugger.dapodil

import cats.Show
import cats.effect._
import cats.effect.std._
import fs2._
import java.io.InputStream
import java.net.URI
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.processors.StateForDebugger
import org.apache.daffodil.sapi.infoset.NullInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream

import logging._

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
      compiler: Compiler
  ): Resource[IO, Parse] =
    for {
      dispatcher <- Dispatcher[IO]
      queue <- Resource.eval(Queue.bounded[IO, Option[Event]](10)) // TODO: explore fs.Channel as alternative
      state <- Resource.eval(State.initial)
      debugger = new Debugger {

        override def init(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            queue.offer(Some(Event.Init(pstate.copyStateForDebugger)))
          }

        override def fini(processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            queue.offer(Some(Event.Fini)) *> queue.offer(None) // terminate the stream with None
          }

        override def startElement(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            queue.offer(
              Some(
                Event.StartElement(
                  pstate.copyStateForDebugger,
                  pstate.currentNode.toScalaOption.map(_.name),
                  pstate.schemaFileLocation
                )
              )
            ) *> state.await // blocks until external control says to unblock, for stepping behavior
          }

        override def endElement(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            queue.offer(Some(Event.EndElement(pstate.copyStateForDebugger)))
          }
      }
      dp <- Resource.eval(compiler.compile(schema).map(p => p.withDebugger(debugger).withDebugging(true)))
      _ <- IO
        .interruptible(true) { // if necessary, kill this thread with extreme prejudice
          dp.parse(
            new InputSourceDataInputStream(data),
            new NullInfosetOutputter()
          )
        }
        .onError(t => IO(s"$t: going to end the queue so consumers can stop").debug() *> IO(t.printStackTrace))
        .guarantee {
          for {
            offered <- queue.tryOffer(None)
            _ <- if (offered) IO.unit else IO.println("! producer couldn't end the queue when shutting down")
          } yield ()
        }
        .background
        .onFinalizeCase {
          case ec @ Resource.ExitCase.Errored(t) => IO(s"parse: $ec").debug() *> IO(t.printStackTrace())
          case ec                                => IO(s"parse: $ec").debug().void
        }
    } yield new Parse {
      def events(): Stream[IO, Event] = Stream.fromQueueNoneTerminated(queue)

      def step(): IO[Unit] = state.step()
      def continue(): IO[Unit] = state.continue()
      def pause(): IO[Unit] = state.pause()
    }

  /** An algebraic data type that reifies the Daffodil `Debugger` callbacks. */
  sealed trait Event

  object Event {
    case class Init(state: StateForDebugger) extends Event
    case class StartElement(state: StateForDebugger, name: Option[String], schemaLocation: SchemaFileLocation)
        extends Event
    case class EndElement(state: StateForDebugger) extends Event
    case object Fini extends Event

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

    def initial(): IO[State] =
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
