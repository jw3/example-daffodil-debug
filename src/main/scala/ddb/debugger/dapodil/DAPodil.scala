package ddb.debugger.dapodil

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
import java.io._
import java.net._
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.util.Misc

import scala.collection.JavaConverters._

import logging._

/** Communication interface to a DAP server while in a connected session. */
trait DAPSession[R, E] {
  def sendResponse(response: R): IO[Unit]
  def sendEvent(event: E): IO[Unit]
}

object DAPSession {
  def apply(server: AbstractProtocolServer): DAPSession[Response, DebugEvent] =
    new DAPSession[Response, DebugEvent] {
      def sendResponse(response: Response): IO[Unit] =
        IO(show"<R $response").debug() *> IO.blocking(server.sendResponse(response))

      def sendEvent(event: DebugEvent): IO[Unit] =
        IO(show"<E $event").debug() *> IO.blocking(server.sendEvent(event))
    }
}

/** Reacts to incoming DAP requests to debug Daffodil schema data processing. */
class DAPodil(
    session: DAPSession[Response, DebugEvent],
    state: Ref[IO, DAPodil.State],
    hotswap: Hotswap[IO, DAPodil.State], // manages those states that have their own resouce management
    compiler: Compiler,
    whenDone: Deferred[IO, Unit]
) extends DAPodil.RequestHandler {
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

    def respondFailure(): Response =
      new Response(request.seq, request.command, false)
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
      case extract(Command.INITIALIZE, _)                                => initialize(request)
      case extract(Command.LAUNCH, _)                                    => launch(request)
      case extract(Command.SETBREAKPOINTS, args: SetBreakpointArguments) => setBreakpoints(request, args)
      case extract(Command.THREADS, _)                                   => threads(request)
      case extract(Command.STACKTRACE, _)                                => stackTrace(request)
      case extract(Command.SCOPES, args: ScopesArguments)                => scopes(request, args)
      case extract(Command.VARIABLES, args: VariablesArguments)          => variables(request, args)
      case extract(Command.NEXT, _)                                      => next(request)
      case extract(Command.CONTINUE, _)                                  => continue(request)
      case extract(Command.PAUSE, _)                                     => pause(request)
      case extract(Command.DISCONNECT, _)                                => disconnect(request)
      case _                                                             => IO(show"! unhandled request $request").debug().void
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
  def launch(request: Request): IO[Unit] =
    // TODO: ensure `launch` is atomic
    state.get.flatMap {
      case DAPodil.State.Initialized =>
        for {
          schema <- schemaURI
          data <- loadData.map(new ByteArrayInputStream(_))

          breakpoints <- Ref[IO].of(DAPodil.Breakpoints(Map.empty))

          launched <- hotswap.swap {
            for {
              parse <- Parse.resource(schema, data, compiler, breakpoints)
              launched <- DAPodil.State.Launched.resource(session, schema, parse, breakpoints)
            } yield launched
          }

          _ <- session.sendResponse(request.respondSuccess())

          // send `Stopped` event to honor `"stopOnEntry":true`
          _ <- session.sendEvent(new Events.StoppedEvent("entry", 1, true))

          _ <- state.set(launched)
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Initialized", s)
    }

  def setBreakpoints(request: Request, args: SetBreakpointArguments): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          _ <- launched.breakpoints.update(bp =>
            bp.copy(value = bp.value + (DAPodil.Path(args.source.path) -> args.breakpoints.toList.map(bp =>
              DAPodil.Line(bp.line)
            ))
            )
          )
          breakpoints = args.breakpoints.toList.map(_ => new Types.Breakpoint(true))
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
          _ <- launched.parse.step
          _ <- session.sendResponse(request.respondSuccess())
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def continue(request: Request): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          _ <- launched.parse.continue()
          _ <- session.sendResponse(request.respondSuccess())
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }

  def pause(request: Request): IO[Unit] =
    state.get.flatMap {
      case launched: DAPodil.State.Launched =>
        for {
          _ <- launched.parse.pause()
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
          state <- launched.state.get
          response = state.stack
            .find(_.stackFrame.id == args.variablesReference)
            .map {
              case DAPodil.Frame(
                  _,
                  dataOffset,
                  childIndex,
                  groupIndex,
                  occursIndex,
                  hidden,
                  foundDelimiter,
                  foundField
                  ) =>
                request.respondSuccess(
                  new Responses.VariablesResponseBody(
                    (List(
                      new Types.Variable("bytePos1b", dataOffset.toString, "number", 0, null),
                      new Types.Variable("hidden", hidden.toString, "bool", 0, null)
                    ) ++ childIndex.map(ci => new Types.Variable("childIndex", ci.toString)).toList
                      ++ groupIndex.map(gi => new Types.Variable("groupIndex", gi.toString)).toList
                      ++ occursIndex.map(oi => new Types.Variable("occursIndex", oi.toString)).toList
                      ++ foundDelimiter.map(fd => new Types.Variable("foundDelimiter", fd)).toList
                      ++ foundField.map(ff => new Types.Variable("foundField", ff)).toList)
                      .sortBy(_.name)
                      .asJava
                  )
                )
            }
            .getOrElse {
              println(show"couldn't find variablesReference ${args.variablesReference} in stack ${state.stack}") // TODO: handle better
              request.respondFailure
            }
          _ <- session.sendResponse(response)
        } yield ()
      case s => DAPodil.InvalidState.raise(request, "Launched", s)
    }
}

object DAPodil extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      state <- Ref[IO].of[State](State.Unitialized)

      address = new InetSocketAddress(4711)
      serverSocket = new ServerSocket(address.getPort, 1, address.getAddress)
      uri = URI.create(s"tcp://${address.getHostString}:${serverSocket.getLocalPort}")

      _ <- IO(s"! waiting at $uri").debug()

      socket <- IO(serverSocket.accept())
      _ <- IO(s"! connected at $uri").debug()

      _ <- DAPodil
        .resource(socket)
        .use(whenDone => whenDone *> IO("whenDone: completed").debug)
      _ <- IO(s"! disconnected at $uri").debug()
    } yield ExitCode.Success

  /** Returns a resource that launches the "DAPodil" debugger that listens on a socket, returning an effect that waits until the debugger stops or the socket closes. */
  def resource(socket: Socket): Resource[IO, IO[Unit]] =
    for {
      dispatcher <- Dispatcher[IO]
      requestHandler <- Resource.eval(Deferred[IO, RequestHandler])
      server <- Server.resource(socket.getInputStream, socket.getOutputStream, dispatcher, requestHandler)
      state <- Resource.eval(Ref[IO].of[State](State.Unitialized))
      hotswap <- Hotswap.create[IO, State].onFinalizeCase(ec => IO(s"hotswap: $ec").debug().void)
      compiler = Compiler.apply
      whenDone <- Resource.eval(Deferred[IO, Unit])
      dapodil = new DAPodil(DAPSession(server), state, hotswap, compiler, whenDone)
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
          _ <- IO(show"R> $request").debug()
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

  /** Models the states of the Daffodil <-> DAP communication. */
  sealed trait State

  object State {
    case object Unitialized extends State
    case object Initialized extends State
    case class Launched(
        schema: URI,
        parse: Parse,
        state: Ref[IO, DaffodilState],
        breakpoints: Ref[IO, Breakpoints]
    ) extends State {
      val stackTrace: IO[StackTrace] = state.get.map(_.stackTrace)
    }

    object Launched {
      def resource(
          session: DAPSession[Response, DebugEvent],
          schema: URI,
          parse: Parse,
          breakpoints: Ref[IO, Breakpoints]
      ): Resource[IO, Launched] =
        for {
          nextFrameId <- Resource.eval(DAPodil.Frame.Id.next)
          current <- Resource.eval(Ref[IO].of(DAPodil.DaffodilState.empty)) // TODO: explore `Stream.holdResource` to convert `Stream[IO, Event]` to `Resource[IO, Signal[IO, Event]]`
          _ <- Resource.eval(session.sendEvent(new Events.ThreadEvent("started", 1L)))
          updateDaffodilState = parse.events
            .through(DAPodil.DaffodilState.fromParse(nextFrameId))
            .evalTap(current.set)
            .onFinalizeCase {
              case ec @ kernel.Resource.ExitCase.Errored(t) =>
                IO(s"updateDaffodilState: $ec").debug() *> IO(t.printStackTrace())
              case ec =>
                IO(s"updateDaffodilState: $ec").debug() *>
                  session.sendEvent(new Events.ThreadEvent("exited", 1L)) *>
                  session.sendEvent(new Events.TerminatedEvent())
            }

          stoppedEventsDelivery = parse.state.discrete.evalTap(deliverStoppedEvents(session))

          launched <- Stream
            .emit(Launched(schema, parse, current, breakpoints))
            .concurrently(updateDaffodilState.merge(stoppedEventsDelivery))
            .evalTap(_ => IO("started Launched"))
            .compile
            .resource
            .lastOrError
            .onFinalizeCase(ec => IO(s"launched: $ec").debug().void)
        } yield launched
    }

    def deliverStoppedEvents(session: DAPSession[Response, DebugEvent]): Parse.State => IO[Unit] = {
      case Parse.State.Stopped(Parse.State.Stopped.Reason.Pause) =>
        session.sendEvent(new Events.StoppedEvent("pause", 1L))
      case Parse.State.Stopped(Parse.State.Stopped.Reason.Step) =>
        session.sendEvent(new Events.StoppedEvent("step", 1L))
      case Parse.State.Stopped(Parse.State.Stopped.Reason.BreakpointHit(_)) =>
        session.sendEvent(new Events.StoppedEvent("breakpoint", 1L))
      case Parse.State.Running => IO.unit // ignore since users initiate continuing a stopped session
    }
  }

  case class InvalidState(request: Request, expected: String, actual: State)
      extends RuntimeException(s"expected state $expected, was $actual when receiving request $request")

  object InvalidState {
    def raise(request: Request, expected: String, actual: State): IO[Nothing] =
      IO.raiseError(InvalidState(request, expected, actual))
  }

  case class DaffodilState(stack: List[Frame]) {
    // there's always a single "thread"
    val thread = new Types.Thread(1L, "daffodil")

    def stackTrace: StackTrace = StackTrace(stack.map(_.stackFrame))

    def push(startElement: Parse.Event.StartElement, nextFrameId: Frame.Id): DaffodilState =
      copy(stack = Frame(startElement, nextFrameId) :: stack)

    def pop(): DaffodilState =
      copy(stack = if (stack.isEmpty) stack else stack.tail) // TODO: warn of bad pop
  }

  object DaffodilState {
    implicit val show: Show[DaffodilState] =
      ds => show"DaffodilState(${ds.stack})"

    val empty = DaffodilState(List.empty)

    /** Translate parse events to updated Daffodil state. */
    def fromParse(
        frameIds: Frame.Id.Next
    ): Stream[IO, Parse.Event] => Stream[IO, DaffodilState] =
      events =>
        events.evalScan(empty) {
          case (_, Parse.Event.Init(_)) => IO.pure(empty)
          case (prev, startElement: Parse.Event.StartElement) =>
            frameIds.next.map(nextFrameId => prev.push(startElement, nextFrameId))
          case (prev, Parse.Event.EndElement(_)) => IO.pure(prev.pop)
          case (prev, _: Parse.Event.Fini.type)  => IO.pure(prev)
        }
  }

  case class Frame(
      stackFrame: Types.StackFrame,
      dataOffset: Long,
      childIndex: Option[Long],
      groupIndex: Option[Long],
      occursIndex: Option[Long],
      hidden: Boolean,
      foundDelimiter: Option[String],
      foundField: Option[String]
  )

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

    /** Transform Daffodil state to a DAP stack frame.
      *
      * @see https://microsoft.github.io/debug-adapter-protocol/specification#Types_StackFrame
      */
    def apply(startElement: Parse.Event.StartElement, id: Id): Frame =
      Frame(
        new Types.StackFrame(
          /* It must be unique across all threads.
           * This id can be used to retrieve the scopes of the frame with the
           * 'scopesRequest' or to restart the execution of a stackframe.
           */
          id.value,
          startElement.name.getOrElse("???"),
          /* If sourceReference > 0 the contents of the source must be retrieved through
           * the SourceRequest (even if a path is specified). */
          new Types.Source(startElement.schemaLocation.uriString, 0),
          startElement.schemaLocation.lineNumber
            .map(_.toInt)
            .getOrElse(1), // line numbers start at 1 according to InitializeRequest
          startElement.schemaLocation.columnNumber
            .map(_.toInt)
            .getOrElse(1) // column numbers start at 1 according to InitializeRequest
        ),
        dataOffset = startElement.state.currentLocation.bytePos1b,
        childIndex = if (startElement.state.childPos != -1) Some(startElement.state.childPos) else None,
        groupIndex = if (startElement.state.groupPos != -1) Some(startElement.state.groupPos) else None,
        occursIndex = if (startElement.state.arrayPos != -1) Some(startElement.state.arrayPos) else None,
        hidden = startElement.state.withinHiddenNest,
        foundDelimiter = for {
          dpr <- startElement.state.delimitedParseResult.toScalaOption
          dv <- dpr.matchedDelimiterValue.toScalaOption
        } yield Misc.remapStringToVisibleGlyphs(dv),
        foundField = for {
          dpr <- startElement.state.delimitedParseResult.toScalaOption
          f <- dpr.field.toScalaOption
        } yield Misc.remapStringToVisibleGlyphs(f)
      )

    implicit val show: Show[Frame] = {
      case Frame(stackFrame, dataOffset, childIndex, groupIndex, occursIndex, hidden, foundDelimiter, foundField) =>
        show"Frame(${stackFrame.id}:${stackFrame.name}, $dataOffset, $childIndex, $groupIndex, $occursIndex, $hidden, $foundDelimiter, $foundField)"
    }
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
    def apply(loc: SchemaFileLocation): Location =
      Location(Path(loc.uriString.drop("file:".length)), Line(loc.lineNumber.map(_.toInt).getOrElse(0)))
  }

  case class Breakpoints(value: Map[Path, List[Line]]) {
    def contains(location: Location): Boolean =
      value.exists {
        case (path, lines) => path == location.path && lines.exists(_ == location.line)
      }
  }
}
