package ddb.debugger.dapodil

import cats.effect._
import cats.effect.kernel.Ref
import cats.effect.std._
import cats.syntax.all._
import com.microsoft.java.debug.core.protocol._
import com.microsoft.java.debug.core.protocol.Messages._
import com.microsoft.java.debug.core.protocol.Events.DebugEvent
import com.microsoft.java.debug.core.protocol.Requests.Command
import java.io._
import java.net._
import java.util.logging.Logger

import scala.collection.JavaConverters._

import logging._

/** Communication interface to a DAP server while in a connected session. */
trait DAPSession[R, E] {
  def sendResponse(response: R): IO[Unit]
  def sendEvent(event: E): IO[Unit]
  def stop(): IO[Unit]
}

object DAPSession {
  def apply(server: AbstractProtocolServer): DAPSession[Response, DebugEvent] =
    new DAPSession[Response, DebugEvent] {
      def sendResponse(response: Response): IO[Unit] =
        IO(println(show"< $response")) *> IO.blocking(server.sendResponse(response))

      def sendEvent(event: DebugEvent): IO[Unit] =
        IO(println(show"E $event")) *> IO.blocking(server.sendEvent(event))

      def stop(): IO[Unit] =
        IO(server.stop())
    }
}

/** Reacts to incoming DAP requests to debug Daffodil schema data processing. */
class DAPodil(
    session: DAPSession[Response, DebugEvent],
    state: Ref[IO, DAPodil.State],
    dispatcher: Dispatcher[IO],
    compiler: Compiler
) {
  // TODO: the DAP server will tell us what schema and data to "debug"
  val schemaURI = IO.blocking(getClass.getResource("/jpeg.dfdl.xsd").toURI)
  val loadData = IO.blocking(getClass.getResourceAsStream("/works.jpg").readAllBytes())

  /** Extension methods to create responses from requests. */
  implicit class RequestSyntax(request: Request) {
    def respondSuccess(body: AnyRef = null): Response = {
      val response = new Response(request.seq, request.command, true)
      response.body = body

      response
    }
  }

  /** Extract a typed command from a string discriminator.
    *
    * TODO: rename extractor
    */
  object extract {
    def unapply(s: String): Some[Command] =
      Some(Command.parse(s))
  }

  /** Respond to requests and optionally update the current state. */
  def dispatch(request: Request): IO[Unit] =
    request.command match {
      case extract(Command.INITIALIZE) => initialize(request)
      case extract(Command.LAUNCH)     => launch(request)
      case extract(Command.THREADS)    => threads(request)
      case extract(Command.STACKTRACE) => stackTrace(request)
      case extract(Command.SCOPES)     => scopes(request)
      case extract(Command.VARIABLES)  => variables(request)
      case extract(Command.NEXT)       => next(request)
      case extract(Command.DISCONNECT) => session.stop()
      case _                           => IO(println(show"! unhandled request $request"))
    }

  /** State.Uninitialized -> State.Initialized */
  def initialize(request: Request): IO[Unit] =
    state.modify {
      case DAPodil.State.Unitialized =>
        val response = request.respondSuccess {
          val caps = new Types.Capabilities()
          caps.supportsConfigurationDoneRequest = true
          new Responses.InitializeResponseBody(caps)
        }
        DAPodil.State.Initialized -> session.sendResponse(response)
      case s => s -> IO.raiseError(new RuntimeException("can only initialize when uninitialized"))
    }.flatten

  /** State.Initialized -> State.Launched */
  def launch(request: Request): IO[Unit] =
    // TODO: ensure `launch` is atomic
    state.get.flatMap {
      case DAPodil.State.Initialized =>
        for {
          schema <- schemaURI
          queue <- Queue.bounded[IO, Unit](1)
          data <- loadData.map(new ByteArrayInputStream(_))
          parse <- Parse(schema, data, dispatcher, queue.take, compiler)

          _ <- session.sendResponse(request.respondSuccess())

          // send `Stopped` event to honor `"stopOnEntry":true`
          event = new Events.StoppedEvent("entry", 1, true)
          _ <- session.sendEvent(event)

          _ <- state.set(DAPodil.State.Launched(schema, parse, queue))
        } yield ()
      case s => DAPodil.InvalidDAPState.raise(request, "Initialized", s)
    }

  def threads(request: Request): IO[Unit] = {
    val response = request.respondSuccess {
      // There's always a single "thread".
      val threads = List(new Types.Thread(1L, "daffodil"))

      new Responses.ThreadsResponseBody(threads.asJava)
    }

    session.sendResponse(response)
  }

  def stackTrace(request: Request): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(_, debugger, _) =>
        for {
          state <- debugger.state
          response = request.respondSuccess(
            new Responses.StackTraceResponseBody(
              state.stackTrace.frames.asJava,
              state.stackTrace.frames.size
            )
          )
          _ <- session.sendResponse(response)
        } yield ()
      case s => DAPodil.InvalidDAPState.raise(request, "Launched", s)
    }

  def next(request: Request): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(_, _, queue) =>
        for {
          _ <- queue.offer(())
          _ <- session.sendResponse(request.respondSuccess())
          _ <- session.sendEvent(new Events.StoppedEvent("step", 1L))
        } yield ()
      case s => DAPodil.InvalidDAPState.raise(request, "Launched", s)
    }

  def scopes(request: Request): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(_, _, _) =>
        session.sendResponse(
          request.respondSuccess(new Responses.ScopesResponseBody(List(new Types.Scope("Daffodil", 1, false)).asJava))
        )
      case s => DAPodil.InvalidDAPState.raise(request, "Launched", s)
    }

  def variables(request: Request): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(_, debugger, _) =>
        for {
          state <- debugger.state()
          response = request.respondSuccess(
            new Responses.VariablesResponseBody(
              List(new Types.Variable("bytePos1b", state.dataOffset.toString, "number", 0, null)).asJava
            )
          )
          _ <- session.sendResponse(response)
        } yield ()
      case s => DAPodil.InvalidDAPState.raise(request, "Launched", s)
    }
}

object DAPodil extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      state <- Ref[IO].of[State](State.Unitialized)

      address = new InetSocketAddress(4711)
      serverSocket = new ServerSocket(address.getPort, 1, address.getAddress)
      uri = URI.create(s"tcp://${address.getHostString}:${serverSocket.getLocalPort}")

      _ = println(s"! waiting at $uri")

      socket <- IO(serverSocket.accept())
      _ = println(s"! connected at $uri")

      _ <- DAPodil(socket).use { dapodilDone =>
        dapodilDone
      }
      _ = println(s"! disconnected at $uri")
    } yield ExitCode.Success

  /** Returns a resource that launches the "DAPodil" debugger that listens on a socket, returning an effect that waits until the debugger stops or the socket closes. */
  def apply(socket: Socket): Resource[IO, IO[OutcomeIO[Unit]]] =
    for {
      dispatcher <- Dispatcher[IO]
      state <- Resource.liftK(Ref[IO].of[State](State.Unitialized))
      compiler = Compiler.apply

      server = new Server(socket.getInputStream, socket.getOutputStream, Logger.getGlobal(), dispatcher)
      dapodil = new DAPodil(DAPSession(server), state, dispatcher, compiler)
      _ = server.dispatch = dapodil.dispatch

      done <- IO.blocking(server.run).onCancel(IO(server.stop())).background
    } yield done

  /** Wraps an AbstractProtocolServer into an IO-based interface. */
  class Server(in: InputStream, out: OutputStream, logger: Logger, dispatcher: Dispatcher[IO])
      extends AbstractProtocolServer(in, out, logger) {
    var dispatch: Request => IO[Unit] = null // TODO: remove super-janky null rendevous

    def dispatchRequest(request: Request): Unit =
      dispatcher.unsafeRunSync {
        IO(println(show"> $request")) *> dispatch(request)
      }
  }

  /** Models the states of the Daffodil <-> DAP communication. */
  sealed trait State

  object State {
    case object Unitialized extends State
    case object Initialized extends State
    case class Launched(schema: URI, parse: Parse, queue: Queue[IO, Unit]) extends State
  }

  case class InvalidDAPState(request: Request, expected: String, actual: State)
      extends RuntimeException(s"expected state $expected, was $actual when receiving request $request")

  object InvalidDAPState {
    def raise(request: Request, expected: String, actual: State): IO[Nothing] =
      IO.raiseError(InvalidDAPState(request, expected, actual))
  }
}
