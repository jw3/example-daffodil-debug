package ddb.debugger.dapodil

import cats.Show
import cats.effect._
import cats.effect.std.Dispatcher
import cats.syntax.all._
import com.microsoft.java.debug.core.protocol.Types
import java.io.InputStream
import java.net.URI
import org.apache.daffodil.sapi._
import org.apache.daffodil.sapi.infoset.NullInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.processors.parsers._

/** A running Daffodil parse. */
trait Parse {
  def state(): IO[Parse.State]
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
      dstate <- Ref[IO].of[State](State(null, StackTrace.empty, 0L))
      parse = new Parse with Debugger {

        override def init(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            for {
              state <- dstate.updateAndGet(Parse.State.fromPState(pstate))
              _ <- IO(println(show"| ${state}"))
            } yield ()
          }

        override def endElement(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            for {
              _ <- await // blocks until external control says to unblock, for stepping behavior
              state <- dstate.updateAndGet(Parse.State.fromPState(pstate))
              _ <- IO(println(show"| ${state}"))
            } yield ()
          }

        def state(): IO[Parse.State] =
          dstate.get
      }

      dp <- compiler.compile(schema).map { p =>
        p.withDebugger(parse).withDebugging(true)
      }
      parsing <- IO
        .blocking {
          dp.parse(
            new InputSourceDataInputStream(data),
            new NullInfosetOutputter()
          )
        }
        .onError(t => IO(t.printStackTrace()))
        .start
      _ <- dstate.set(State(parsing, StackTrace.empty, 0))
    } yield parse

  case class State(parsing: FiberIO[ParseResult], stackTrace: StackTrace, dataOffset: Long) {
    // there's always a single "thread"
    val thread = new Types.Thread(1L, "daffodil")
  }

  object State {
    def fromPState(pstate: PState): State => State =
      s => s.copy(stackTrace = StackTrace(pstate), dataOffset = pstate.currentLocation.bytePos1b)

    implicit val show: Show[State] =
      state => show"State(${state.stackTrace}, ${state.dataOffset})"
  }

  case class StackTrace(frames: List[Types.StackFrame])

  object StackTrace {
    def apply(state: PState): StackTrace =
      StackTrace(
        List(
          // https://microsoft.github.io/debug-adapter-protocol/specification#Types_StackFrame
          new Types.StackFrame(
            /* It must be unique across all threads.
             * This id can be used to retrieve the scopes of the frame with the
             * 'scopesRequest' or to restart the execution of a stackframe.
             */
            0, // TODO: unique stack frame id
            "processor",
            new Types.Source(state.schemaFileLocation.uriString, 0),
            state.schemaFileLocation.lineNumber
              .map(_.toInt)
              .getOrElse(1), // line numbers start at 1 according to InitializeRequest
            state.schemaFileLocation.columnNumber
              .map(_.toInt)
              .getOrElse(1) // column numbers start at 1 according to InitializeRequest
          )
        )
      )

    val empty: StackTrace = StackTrace(List.empty)

    implicit val show: Show[StackTrace] =
      st => st.frames.map(f => s"${f.line}:${f.column}").mkString("; ")
  }
}
