package ddb.debugger.dapodil

import cats.data._
import cats.effect._
import cats.effect.kernel.Ref
import cats.effect.std._
import cats.Show
import cats.syntax.all._
import com.microsoft.java.debug.core.protocol._
import com.microsoft.java.debug.core.protocol.Messages._
import com.microsoft.java.debug.core.protocol.Events.DebugEvent
import com.microsoft.java.debug.core.protocol.Requests._
import fs2._
import fs2.concurrent.Signal
import java.io._
import java.net._
import java.nio.file
import java.nio.file.Paths
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.JavaConverters._

import logging._

/** Communication interface to a DAP server while in a connected session. */
trait DAPSession[R, E] {
  def sendResponse(response: R): IO[Unit]
  def sendEvent(event: E): IO[Unit]
}

object DAPSession {
  implicit val logger: Logger[IO] = Slf4jLogger.getLogger

  def apply(server: AbstractProtocolServer): DAPSession[Response, DebugEvent] =
    new DAPSession[Response, DebugEvent] {
      def sendResponse(response: Response): IO[Unit] =
        Logger[IO].debug(show"<R $response") *> IO.blocking(server.sendResponse(response))

      def sendEvent(event: DebugEvent): IO[Unit] =
        Logger[IO].debug(show"<E $event") *> IO.blocking(server.sendEvent(event))
    }
}

/** Connect a debugee to an external debugger via DAP. */
class DAPodil(
    session: DAPSession[Response, DebugEvent],
    state: Ref[IO, DAPodil.State],
    hotswap: Hotswap[IO, DAPodil.State], // manages those states that have their own resouce management
    debugee: DAPodil.LaunchArgs => Resource[IO, DAPodil.Debugee],
    whenDone: Deferred[IO, Unit]
) extends DAPodil.RequestHandler {
  implicit val logger: Logger[IO] = Slf4jLogger.getLogger

  /** Extension methods to create responses from requests. */
  implicit class RequestSyntax(request: Request) {
    def respondSuccess(body: AnyRef = null): Response = {
      val response = new Response(request.seq, request.command, true)
      response.body = body

      response
    }

    def respondFailure(message: Option[String] = None): Response =
      message.fold(new Response(request.seq, request.command, false))(
        new Response(request.seq, request.command, false, _)
      )
  }

  /** Extract a typed command from a string discriminator.
    *
    * TODO: rename extractor
    */
  object extract {
    def unapply(request: Request): Some[(Command, Arguments)] =
      Some {
        val command = Command.parse(request.command)
        command -> JsonUtils.fromJson(request.arguments, command.getArgumentType())
      }
  }

  /** Respond to requests and optionally update the current state. */
  def handle(request: Request): IO[Unit] =
    request match {
      case extract(Command.INITIALIZE, _) => initialize(request)
      case extract(Command.LAUNCH, _)     =>
        // We ignore the java-debug LaunchArguments because it is overspecialized for JVM debugging, and parse our own LaunchArgs type.
        DAPodil.LaunchArgs
          .parse(request)
          .fold(
            errors =>
              Logger[IO].warn(show"error parsing launch args: ${errors.mkString_(", ")}") *> session
                .sendResponse(request.respondFailure(Some(show"error parsing launch args: ${errors.mkString_(", ")}"))),
            args => launch(request, args)
          )
      case extract(Command.SETBREAKPOINTS, args: SetBreakpointArguments) => setBreakpoints(request, args)
      case extract(Command.THREADS, _)                                   => threads(request)
      case extract(Command.STACKTRACE, _)                                => stackTrace(request)
      case extract(Command.SCOPES, args: ScopesArguments)                => scopes(request, args)
      case extract(Command.VARIABLES, args: VariablesArguments)          => variables(request, args)
      case extract(Command.NEXT, _)                                      => next(request)
      case extract(Command.CONTINUE, _)                                  => continue(request)
      case extract(Command.PAUSE, _)                                     => pause(request)
      case extract(Command.DISCONNECT, _)                                => disconnect(request)
      case _                                                             => Logger[IO].warn(show"unhandled request $request") *> session.sendResponse(request.respondFailure())
    }

  /** State.Uninitialized -> State.Initialized */
  def initialize(request: Request): IO[Unit] =
    state.modify {
      case DAPodil.State.Unitialized =>
        val response = request.respondSuccess(new Responses.InitializeResponseBody(new Types.Capabilities()))
        DAPodil.State.Initialized -> (session.sendResponse(response) *> session.sendEvent(
          new Events.InitializedEvent()
        ))
      case s => s -> IO.raiseError(new RuntimeException("can only initialize when uninitialized"))
    }.flatten

  /** State.Initialized -> State.Launched */
  def launch(request: Request, args: DAPodil.LaunchArgs): IO[Unit] =
    // TODO: ensure `launch` is atomic
    state.get.flatMap {
      case DAPodil.State.Initialized =>
        for {
          launched <- hotswap.swap {
            for {
              dbgee <- debugee(args)
              launched <- DAPodil.State.Launched.resource(session, dbgee)
            } yield launched
          }.attempt

          _ <- launched match {
            case Left(t) =>
              Logger[IO].warn(t)(show"couldn't launch, request $request") *>
                session.sendResponse(
                  request.respondFailure(Some(show"Couldn't launch Daffodil debugger: ${t.getMessage()}"))
                )
            case Right(launchedState) =>
              for {
                _ <- session.sendResponse(request.respondSuccess())

                // send `Stopped` event to honor `"stopOnEntry":true`
                _ <- session.sendEvent(new Events.StoppedEvent("entry", 1, true))

                _ <- state.set(launchedState)
              } yield ()
          }
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Initialized", s)
    }

  def setBreakpoints(request: Request, args: SetBreakpointArguments): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          _ <- launched.debugee.setBreakpoints(
            DAPodil.Path(args.source.path) -> args.breakpoints.toList.map(bp => DAPodil.Line(bp.line))
          )
          breakpoints = args.breakpoints.toList.zipWithIndex.map {
            case (bp, i) => new Types.Breakpoint(i, true, bp.line, "")
          }
          response = request.respondSuccess(
            new Responses.SetBreakpointsResponseBody(breakpoints.asJava)
          )
          _ <- session.sendResponse(response)
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
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
      case launched: DAPodil.State.Launched =>
        for {
          stackTrace <- launched.stackTrace
          response = request.respondSuccess(
            new Responses.StackTraceResponseBody(
              stackTrace.frames.asJava,
              stackTrace.frames.size
            )
          )
          _ <- session.sendResponse(response)
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def next(request: Request): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          _ <- launched.debugee.step
          _ <- session.sendResponse(request.respondSuccess())
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def continue(request: Request): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          _ <- launched.debugee.continue()
          _ <- session.sendResponse(request.respondSuccess())
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def pause(request: Request): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          _ <- launched.debugee.pause()
          _ <- session.sendResponse(request.respondSuccess())
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def disconnect(request: Request): IO[Unit] =
    session
      .sendResponse(request.respondSuccess())
      .guarantee(hotswap.clear *> whenDone.complete(()).void) // TODO: new state: disconnected

  def scopes(request: Request, args: ScopesArguments): IO[Unit] =
    state.get.flatMap {
      case _: DAPodil.State.Launched =>
        session.sendResponse(
          request.respondSuccess(
            // there's only one scope per stack frame
            new Responses.ScopesResponseBody(List(new Types.Scope("Daffodil", args.frameId, false)).asJava)
          )
        )
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def variables(request: Request, args: VariablesArguments): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        // return the variables for the requested "variablesReference", which is associated with a scope, which is associated with a stack frame
        for {
          state <- launched.debugee.data.get
          response = state.stack
            .find(_.stackFrame.id == args.variablesReference)
            .map {
              case DAPodil.Frame(_, variables) =>
                request.respondSuccess(
                  new Responses.VariablesResponseBody(variables.asJava)
                )
            }
          _ <- response.fold(
            Logger[IO]
              .warn(show"couldn't find variablesReference ${args.variablesReference} in stack ${state.stack}") *> // TODO: handle better
              session.sendResponse(request.respondFailure())
          )(session.sendResponse)
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }
}

object DAPodil extends IOApp {

  implicit val logger: Logger[IO] = Slf4jLogger.getLogger

  def run(args: List[String]): IO[ExitCode] =
    for {
      state <- Ref[IO].of[State](State.Unitialized)

      address = new InetSocketAddress(4711)
      serverSocket = new ServerSocket(address.getPort, 1, address.getAddress)
      uri = URI.create(s"tcp://${address.getHostString}:${serverSocket.getLocalPort}")

      _ <- Logger[IO].info(s"waiting at $uri")

      socket <- IO(serverSocket.accept())
      _ <- Logger[IO].info(s"connected at $uri")

      debugee = (args: DAPodil.LaunchArgs) =>
        for {
          schema <- Resource.eval(IO.blocking(args.schemaPath.toUri))
          data <- Resource.eval(
            IO.blocking(new FileInputStream(args.dataPath.toFile).readAllBytes())
              .map(new ByteArrayInputStream(_))
          )
          debugee <- Parse.resource(schema, data, Compiler())
        } yield debugee

      _ <- DAPodil
        .resource(socket, debugee)
        .use(whenDone => whenDone *> Logger[IO].debug("whenDone: completed"))
      _ <- Logger[IO].info(s"disconnected at $uri")
    } yield ExitCode.Success

  /** Returns a resource that launches the "DAPodil" debugger that listens on a socket, returning an effect that waits until the debugger stops or the socket closes. */
  def resource(socket: Socket, debugee: LaunchArgs => Resource[IO, Debugee]): Resource[IO, IO[Unit]] =
    for {
      dispatcher <- Dispatcher[IO]
      requestHandler <- Resource.eval(Deferred[IO, RequestHandler])
      server <- Server.resource(socket.getInputStream, socket.getOutputStream, dispatcher, requestHandler)
      state <- Resource.eval(Ref[IO].of[State](State.Unitialized))
      hotswap <- Hotswap.create[IO, State].onFinalizeCase(ec => Logger[IO].debug(s"hotswap: $ec"))
      whenDone <- Resource.eval(Deferred[IO, Unit])
      dapodil = new DAPodil(DAPSession(server), state, hotswap, debugee, whenDone)
      _ <- Resource.eval(requestHandler.complete(dapodil))
    } yield whenDone.get

  case class LaunchArgs(schemaPath: file.Path, dataPath: file.Path) extends Arguments

  object LaunchArgs {
    def parse(request: Request): EitherNel[String, LaunchArgs] =
      (
        Option(request.arguments.getAsJsonPrimitive("program"))
          .toRight("missing 'program' field from launch request")
          .flatMap(path =>
            Either
              .catchNonFatal(Paths.get(path.getAsString))
              .leftMap(t => s"'program' field from launch request is not a valid path: $t")
          )
          .toEitherNel,
        Option(request.arguments.getAsJsonPrimitive("data"))
          .toRight("missing 'data' field from launch request")
          .flatMap(path =>
            Either
              .catchNonFatal(Paths.get(path.getAsString))
              .leftMap(t => s"'data' field from launch request is not a valid path: $t")
          )
          .toEitherNel
      ).parMapN(LaunchArgs.apply)
  }

  trait RequestHandler {
    def handle(request: Request): IO[Unit]
  }

  /** Wraps an AbstractProtocolServer into an IO-based interface. */
  class Server(
      in: InputStream,
      out: OutputStream,
      dispatcher: Dispatcher[IO],
      requestHandler: Deferred[IO, RequestHandler]
  ) extends AbstractProtocolServer(in, out) {
    def dispatchRequest(request: Request): Unit =
      dispatcher.unsafeRunSync {
        for {
          _ <- Logger[IO].debug(show"R> $request")
          handler <- requestHandler.get
          _ <- handler.handle(request)
        } yield ()
      }
  }

  object Server {

    /** Starts an `AbstractProtocolServer` for the lifetime of the resource, stopping it upon release. */
    def resource(
        in: InputStream,
        out: OutputStream,
        dispatcher: Dispatcher[IO],
        requestHandler: Deferred[IO, RequestHandler]
    ): Resource[IO, AbstractProtocolServer] =
      Resource
        .make(IO(new Server(in, out, dispatcher, requestHandler)))(server => IO(server.stop()))
        .flatTap(server => IO(server.run).background)
  }

  /** DAPodil launches the debugee which reports its state and handles debug commands. */
  trait Debugee {
    // TODO: extract "control" interface from "state" interface
    def data(): Signal[IO, Data]
    def state(): Stream[IO, Debugee.State]
    def outputs(): Stream[IO, Events.OutputEvent]

    def step(): IO[Unit]
    def continue(): IO[Unit]
    def pause(): IO[Unit]
    def setBreakpoints(breakpoints: (Path, List[Line])): IO[Unit]
  }

  object Debugee {
    sealed trait State

    object State {
      case object Running extends State
      case class Stopped(reason: Stopped.Reason) extends State

      object Stopped {
        sealed trait Reason

        object Reason {
          case object Pause extends Reason
          case object Step extends Reason
          case class BreakpointHit(location: DAPodil.Location) extends Reason
        }
      }
    }
  }

  /** Models the states of the Daffodil <-> DAP communication. */
  sealed trait State

  object State {
    case object Unitialized extends State
    case object Initialized extends State
    case class Launched(debugee: Debugee) extends State {
      val stackTrace: IO[StackTrace] = debugee.data.get.map(_.stackTrace)
    }

    implicit val show: Show[State] = Show.fromToString

    object Launched {
      def resource(
          session: DAPSession[Response, DebugEvent],
          debugee: Debugee
      ): Resource[IO, Launched] =
        for {
          _ <- Resource.eval(session.sendEvent(new Events.ThreadEvent("started", 1L)))

          stoppedEventsDelivery = debugee.state
            .collect { case s: Debugee.State.Stopped => s }
            .evalMap(deliverStoppedEvents(session))
            .onFinalizeCase {
              case ec @ kernel.Resource.ExitCase.Errored(t) =>
                Logger[IO].debug(s"deliverStoppedEvents: $ec") *> IO(t.printStackTrace())
              case ec =>
                Logger[IO].debug(s"deliverStoppedEvents: $ec") *>
                  session.sendEvent(new Events.ThreadEvent("exited", 1L)) *>
                  session.sendEvent(new Events.TerminatedEvent())
            }

          outputEventsDelivery = debugee.outputs.evalMap(session.sendEvent)

          launched <- Stream
            .emit(Launched(debugee))
            .concurrently(stoppedEventsDelivery.merge(outputEventsDelivery))
            .evalTap(_ => Logger[IO].debug("started Launched"))
            .compile
            .resource
            .lastOrError
            .onFinalizeCase(ec => Logger[IO].debug(s"launched: $ec"))
        } yield launched
    }

    def deliverStoppedEvents(session: DAPSession[Response, DebugEvent]): Debugee.State.Stopped => IO[Unit] = {
      case Debugee.State.Stopped(Debugee.State.Stopped.Reason.Pause) =>
        session.sendEvent(new Events.StoppedEvent("pause", 1L))
      case Debugee.State.Stopped(Debugee.State.Stopped.Reason.Step) =>
        session.sendEvent(new Events.StoppedEvent("step", 1L))
      case Debugee.State.Stopped(Debugee.State.Stopped.Reason.BreakpointHit(_)) =>
        session.sendEvent(new Events.StoppedEvent("breakpoint", 1L))
    }
  }

  case class InvalidState(request: Request, expected: String, actual: State)
      extends RuntimeException(show"expected state $expected, was $actual when receiving request $request")

  object InvalidState {
    def raise(request: Request, expected: String, actual: State): IO[Nothing] =
      IO.raiseError(InvalidState(request, expected, actual))
  }

  // TODO: map multiple threads to stack
  case class Data(stack: List[Frame]) {
    // there's always a single "thread"
    val thread = new Types.Thread(1L, "daffodil")

    def stackTrace: StackTrace = StackTrace(stack.map(_.stackFrame))

    def push(frame: Frame): Data =
      copy(stack = frame :: stack)

    def pop(): Data =
      copy(stack = if (stack.isEmpty) stack else stack.tail) // TODO: warn of bad pop
  }

  object Data {
    implicit val show: Show[Data] =
      ds => show"DaffodilState(${ds.stack})"

    val empty = Data(List.empty)
  }

  case class Frame(stackFrame: Types.StackFrame, variables: List[Types.Variable])

  object Frame {

    /** Identifier for a stack frame within a stack trace. */
    case class Id(value: Int) extends AnyVal

    object Id {

      /** A source of new frame identifiers. */
      trait Next {
        def next: IO[Id]
      }

      def next: IO[Next] =
        for {
          ids <- Ref[IO].of(0)
        } yield new Next {
          def next: IO[Id] = ids.getAndUpdate(_ + 1).map(Id.apply)
        }
    }

    implicit val show: Show[Frame] = Show.fromToString
  }

  case class StackTrace(frames: List[Types.StackFrame]) {
    def push(frame: Types.StackFrame): StackTrace = copy(frame :: frames)
    def pop(): StackTrace = copy(frames.tail)
  }

  object StackTrace {
    val empty: StackTrace = StackTrace(List.empty)

    implicit val show: Show[StackTrace] =
      st => st.frames.map(f => s"${f.line}:${f.column}").mkString("; ")
  }

  case class Path(value: String) extends AnyVal
  case class Line(value: Int) extends AnyVal
  case class Location(path: Path, line: Line)

  object Location {
    implicit val show: Show[Location] = Show.fromToString
  }

  case class Breakpoints(value: Map[Path, List[Line]]) {
    def contains(location: Location): Boolean =
      value.exists {
        case (path, lines) => path == location.path && lines.exists(_ == location.line)
      }
  }

  object Breakpoints {
    val empty: Breakpoints = Breakpoints(Map.empty)

    implicit val show: Show[Breakpoints] = Show.fromToString
  }
}
