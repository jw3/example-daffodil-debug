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
}

object Parse {
  def apply(
      schema: URI,
      data: InputStream,
      dispatcher: Dispatcher[IO],
      await: IO[Unit],
      compiler: Compiler
  ): IO[Parse] =
    for {
      queue <- Queue.bounded[IO, Event](10)
      debugger = new Debugger {

        override def init(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            queue.offer(Event.Init(pstate, processor))
          }

        override def startElement(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            for {
              _ <- queue.offer(Event.StartElement(pstate, processor))
              _ <- await // blocks until external control says to unblock, for stepping behavior
            } yield ()
          }

        override def endElement(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            queue.offer(Event.EndElement(pstate, processor))
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
      def events(): Stream[IO, Event] = Stream.fromQueueUnterminated(queue)
    }

  sealed trait Event

  // TODO: it's probably very dubious to assume PState and Parser are immutable
  object Event {
    case class Init(pstate: PState, parser: Parser) extends Event
    case class StartElement(pstate: PState, parser: Parser) extends Event
    case class EndElement(pstate: PState, parser: Parser) extends Event

    implicit val show: Show[Event] = Show.fromToString
  }

}
