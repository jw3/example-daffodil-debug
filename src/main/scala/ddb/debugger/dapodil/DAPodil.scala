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
    debugee: Request => EitherNel[String, Resource[IO, DAPodil.Debugee]],
    whenDone: Deferred[IO, DAPodil.Done]
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
    // TODO: java-debug doesn't seem to support the Restart request
    request match {
      case extract(Command.INITIALIZE, _) => initialize(request)
      case extract(Command.LAUNCH, _)     =>
        // We ignore the java-debug LaunchArguments because it is overspecialized for JVM debugging, and parse our own.
        launch(request)
      case extract(Command.SETBREAKPOINTS, args: SetBreakpointArguments) => setBreakpoints(request, args)
      case extract(Command.THREADS, _)                                   => threads(request)
      case extract(Command.STACKTRACE, _)                                => stackTrace(request)
      case extract(Command.SCOPES, args: ScopesArguments)                => scopes(request, args)
      case extract(Command.VARIABLES, args: VariablesArguments)          => variables(request, args)
      case extract(Command.NEXT, _)                                      => next(request)
      case extract(Command.CONTINUE, _)                                  => continue(request)
      case extract(Command.PAUSE, _)                                     => pause(request)
      case extract(Command.DISCONNECT, args: DisconnectArguments)        => disconnect(request, args)
      case extract(Command.EVALUATE, args: EvaluateArguments)            => eval(request, args)
      case _                                                             => Logger[IO].warn(show"unhandled request $request") *> session.sendResponse(request.respondFailure())
    }

  /** State.Uninitialized -> State.Initialized */
  def initialize(request: Request): IO[Unit] =
    state.modify {
      case DAPodil.State.Uninitialized =>
        // TODO: VSCode doesn't seem to notice supportsRestartRequest=true and sends Disconnect (+restart) requests instead
        val response = request.respondSuccess(new Responses.InitializeResponseBody(new Types.Capabilities()))
        DAPodil.State.Initialized -> (session.sendResponse(response) *> session.sendEvent(
          new Events.InitializedEvent()
        ))
      case s => s -> IO.raiseError(new RuntimeException("can only initialize when uninitialized"))
    }.flatten

  /** State.Initialized -> State.Launched */
  def launch(request: Request): IO[Unit] =
    // TODO: ensure `launch` is atomic
    state.get.flatMap {
      case DAPodil.State.Initialized =>
        debugee(request) match {
          case Left(errors) =>
            Logger[IO].warn(show"error parsing launch args: ${errors.mkString_(", ")}") *> session
              .sendResponse(request.respondFailure(Some(show"error parsing launch args: ${errors.mkString_(", ")}")))
          case Right(dbgee) =>
            for {
              launched <- hotswap.swap {
                DAPodil.State.Launched.resource(session, dbgee)
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
        }
      case s => DAPodil.InvalidState.raise(request, "Initialized", s)
    }

  def setBreakpoints(request: Request, args: SetBreakpointArguments): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(debugee) =>
        for {
          _ <- debugee.setBreakpoints(
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

  def threads(request: Request): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          threads <- launched.threads
          _ <- session.sendResponse(
            request.respondSuccess(
              new Responses.ThreadsResponseBody(threads.asJava)
            )
          )
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def stackTrace(request: Request): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          stackTrace <- launched.stackTrace
          response = request.respondSuccess(
            new Responses.StackTraceResponseBody(
              stackTrace.frames.map(_.stackFrame).asJava,
              stackTrace.frames.size
            )
          )
          _ <- session.sendResponse(response)
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def next(request: Request): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(debugee) =>
        for {
          _ <- debugee.step
          _ <- session.sendResponse(request.respondSuccess())
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def continue(request: Request): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(debugee) =>
        for {
          _ <- debugee.continue()
          _ <- session.sendResponse(request.respondSuccess())
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def pause(request: Request): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(debugee) =>
        for {
          _ <- debugee.pause()
          _ <- session.sendResponse(request.respondSuccess())
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def disconnect(request: Request, args: DisconnectArguments): IO[Unit] =
    session
      .sendResponse(request.respondSuccess())
      .guarantee {
        hotswap.clear *> whenDone.complete(DAPodil.Done(args.restart)).void
      }

  def scopes(request: Request, args: ScopesArguments): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(debugee) =>
        for {
          data <- debugee.data.get
          _ <- data.stack
            .findFrame(DAPodil.Frame.Id(args.frameId))
            .fold(
              session.sendResponse(request.respondFailure(Some(s"couldn't find scopes for frame ${args.frameId}")))
            ) { frame =>
              session.sendResponse(
                request.respondSuccess(new Responses.ScopesResponseBody(frame.scopes.map(_.toDAP()).asJava))
              )
            }
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def variables(request: Request, args: VariablesArguments): IO[Unit] =
    state.get.flatMap {
      case DAPodil.State.Launched(debugee) =>
        // return the variables for the requested "variablesReference", which is associated with a scope, which is associated with a stack frame
        for {
          data <- debugee.data.get
          _ <- data.stack
            .variables(DAPodil.VariablesReference(args.variablesReference))
            .fold(
              Logger[IO]
                .warn(show"couldn't find variablesReference ${args.variablesReference} in stack ${data}") *> // TODO: handle better
                session.sendResponse(request.respondFailure())
            )(variables =>
              session.sendResponse(request.respondSuccess(new Responses.VariablesResponseBody(variables.asJava)))
            )
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def eval(request: Request, args: EvaluateArguments): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          variable <- launched.debugee.eval(args)
          _ <- variable match {
            case None => session.sendResponse(request.respondFailure())
            case Some(v) =>
              session.sendResponse(request.respondSuccess(new Responses.EvaluateResponseBody(v.value, 0, null, 0)))
          }
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }
}

object DAPodil extends IOApp {

  implicit val logger: Logger[IO] = Slf4jLogger.getLogger

  def run(args: List[String]): IO[ExitCode] =
    for {
      state <- Ref[IO].of[State](State.Uninitialized)

      address = new InetSocketAddress(4711)
      serverSocket = new ServerSocket(address.getPort, 1, address.getAddress)
      uri = URI.create(s"tcp://${address.getHostString}:${serverSocket.getLocalPort}")

      _ <- listen(serverSocket, uri).iterateWhile((_.restart))

    } yield ExitCode.Success

  def listen(socket: ServerSocket, uri: URI): IO[Done] =
    for {
      _ <- Logger[IO].info(s"waiting at $uri")
      socket <- IO.blocking(socket.accept())
      _ <- Logger[IO].info(s"connected at $uri")

      done <- DAPodil
        .resource(socket, Parse.debugee)
        .use(whenDone => Logger[IO].debug("whenDone: completed") *> whenDone)
      _ <- Logger[IO].info(s"disconnected at $uri")
    } yield done

  case class Done(restart: Boolean = false)

  /** Returns a resource that launches the "DAPodil" debugger that listens on a socket, returning an effect that waits until the debugger stops or the socket closes. */
  def resource(socket: Socket, debugee: Request => EitherNel[String, Resource[IO, Debugee]]): Resource[IO, IO[Done]] =
    for {
      dispatcher <- Dispatcher[IO]
      requestHandler <- Resource.eval(Deferred[IO, RequestHandler])
      server <- Server.resource(socket.getInputStream, socket.getOutputStream, dispatcher, requestHandler)
      state <- Resource.eval(Ref[IO].of[State](State.Uninitialized))
      hotswap <- Hotswap.create[IO, State].onFinalizeCase(ec => Logger[IO].debug(s"hotswap: $ec"))
      whenDone <- Resource.eval(Deferred[IO, Done])
      dapodil = new DAPodil(DAPSession(server), state, hotswap, debugee, whenDone)
      _ <- Resource.eval(requestHandler.complete(dapodil))
    } yield whenDone.get

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
          _ <- handler.handle(request).recoverWith {
            case t => Logger[IO].error(t)(show"error during handling of request $request")
          }
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
        .flatTap(server => IO.blocking(server.run).background)
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
    def eval(args: EvaluateArguments): IO[Option[Types.Variable]]
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
    case object Uninitialized extends State
    case object Initialized extends State
    case class Launched(debugee: Debugee) extends State {
      val stackTrace: IO[StackTrace] = debugee.data.get.map(_.stack)
      val threads: IO[List[Types.Thread]] = debugee.data.get.map(_.threads)
    }

    object Launched {
      def resource(
          session: DAPSession[Response, DebugEvent],
          debugee: Resource[IO, Debugee]
      ): Resource[IO, Launched] =
        for {
          _ <- Resource.eval(session.sendEvent(new Events.ThreadEvent("started", 1L)))
          debugee <- debugee.onFinalizeCase(ec => Logger[IO].debug(s"debugee: $ec"))

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

          outputEventsDelivery = debugee.outputs
            .evalMap(session.sendEvent)
            .onFinalizeCase(ec => Logger[IO].debug(s"outputEventsDelivery: $ec"))

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

    implicit val show: Show[State] = Show.fromToString

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

  case class Data(stack: StackTrace) {
    // there's always a single "thread"
    val threads = List(new Types.Thread(1L, "daffodil"))

    def push(frame: Frame): Data =
      copy(stack = stack.push(frame))

    def pop(): Data =
      copy(stack = stack.pop) // TODO: warn of bad pop
  }

  object Data {
    implicit val show: Show[Data] =
      ds => show"DaffodilState(${ds.stack})"

    val empty: Data = Data(StackTrace.empty)
  }

  case class Frame(id: Frame.Id, stackFrame: Types.StackFrame, scopes: List[Frame.Scope]) {
    def variables(reference: VariablesReference): Option[List[Types.Variable]] =
      scopes.collectFirstSome(_.variables.get(reference))
  }

  object Frame {

    implicit val show: Show[Frame] = Show.fromToString

    /** Identifier for a stack frame within a stack trace. */
    case class Id(value: Int) extends AnyVal

    case class Scope(
        name: String,
        reference: VariablesReference,
        variables: Map[VariablesReference, List[Types.Variable]]
    ) {
      def toDAP(): Types.Scope =
        new Types.Scope(name, reference.value, false)
    }
  }

  case class VariablesReference(value: Int) extends AnyVal

  case class StackTrace(frames: List[Frame]) {
    def push(frame: Frame): StackTrace =
      copy(frame :: frames)

    def pop(): StackTrace =
      copy(frames.tail)

    def findFrame(frameId: Frame.Id): Option[Frame] =
      frames.find(_.id == frameId)

    def variables(reference: VariablesReference): Option[List[Types.Variable]] =
      frames.collectFirstSome(_.variables(reference))
  }

  object StackTrace {
    val empty: StackTrace = StackTrace(List.empty)

    implicit val show: Show[StackTrace] =
      st => st.frames.map(f => s"${f.stackFrame.line}:${f.stackFrame.column}").mkString("; ")
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
