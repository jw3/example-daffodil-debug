package ddb.debugger.dapodil

import cats.Show
import cats.effect._
import cats.effect.std._
import cats.syntax.all._
import com.microsoft.java.debug.core.protocol.Types
import fs2._
import fs2.concurrent.Signal
import java.io.InputStream
import java.net.URI
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.processors.StateForDebugger
import org.apache.daffodil.sapi.infoset.NullInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import org.apache.daffodil.util.Misc
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** A running Daffodil parse. */
object Parse {
  implicit val logger: Logger[IO] = Slf4jLogger.getLogger

  def resource(
      schema: URI,
      data: InputStream,
      compiler: Compiler
  ): Resource[IO, DAPodil.Debugee] =
    for {
      dispatcher <- Dispatcher[IO]
      // TODO: explore fs.Channel as alternative to Queue
      parseEvents <- Resource.eval(Queue.bounded[IO, Option[Event]](10))
      parseState <- Resource.eval(Queue.bounded[IO, Option[DAPodil.Debugee.State]](10))
      control <- Resource.eval(Control.initial())
      breakpoints <- Resource.eval(Ref[IO].of(DAPodil.Breakpoints.empty))
      debugger = new Debugger {

        override def init(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            parseEvents.offer(Some(Event.Init(pstate.copyStateForDebugger)))
          }

        override def fini(processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            parseEvents.offer(Some(Event.Fini)) *> parseEvents.offer(None) *> parseState.offer(None) // terminate the queues with None
          }

        override def startElement(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            val push = Some(
              Event.StartElement(
                pstate.copyStateForDebugger,
                pstate.currentNode.toScalaOption.map(_.name),
                pstate.schemaFileLocation
              )
            )
            logger.debug("pre-offer") *>
              parseEvents.offer(push) *>
              logger.debug("pre-checkBreakpoints") *>
              checkBreakpoints(createLocation(pstate.schemaFileLocation)) *>
              logger.debug("pre-control await") *>
              control.await() *> // may block until external control says to unblock, for stepping behavior
              logger.debug("post-control await")
          }

        def checkBreakpoints(location: DAPodil.Location): IO[Unit] =
          for {
            bp <- breakpoints.get
            _ <- Logger[IO].debug(show"check break at location $location, breakpoints $bp")
            _ <- if (bp.contains(location))
              control.pause() *> parseState.offer(
                Some(DAPodil.Debugee.State.Stopped(DAPodil.Debugee.State.Stopped.Reason.BreakpointHit(location)))
              )
            else IO.unit
          } yield ()

        override def endElement(pstate: PState, processor: Parser): Unit =
          dispatcher.unsafeRunSync {
            parseEvents.offer(Some(Event.EndElement(pstate.copyStateForDebugger)))
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
        .onError(t => Logger[IO].debug(s"$t: going to end the queue so consumers can stop") *> IO(t.printStackTrace))
        .guarantee {
          for {
            offered <- parseEvents.tryOffer(None)
            _ <- if (offered) IO.unit else Logger[IO].warn("producer couldn't end the queue when shutting down")
          } yield ()
        }
        .background
        .onFinalizeCase {
          case ec @ Resource.ExitCase.Errored(t) => Logger[IO].error(t)(s"parse: $ec")
          case ec                                => Logger[IO].debug(s"parse: $ec")
        }

      nextFrameId <- Resource.eval(DAPodil.Frame.Id.next)
      debugeeData <- Stream
        .fromQueueNoneTerminated(parseEvents)
        .through(fromParse(nextFrameId))
        .holdResource(DAPodil.Data.empty)
    } yield new DAPodil.Debugee {
      def data(): Signal[IO, DAPodil.Data] =
        debugeeData

      def state(): Stream[IO, DAPodil.Debugee.State] =
        Stream.fromQueueNoneTerminated(parseState)

      def step(): IO[Unit] =
        control.step() *> parseState.offer(
          Some(DAPodil.Debugee.State.Stopped(DAPodil.Debugee.State.Stopped.Reason.Step))
        )

      def continue(): IO[Unit] =
        control.continue() *> parseState.offer(Some(DAPodil.Debugee.State.Running))

      def pause(): IO[Unit] =
        control.pause() *> parseState.offer(
          Some(DAPodil.Debugee.State.Stopped(DAPodil.Debugee.State.Stopped.Reason.Pause))
        )

      def setBreakpoints(args: (DAPodil.Path, List[DAPodil.Line])): IO[Unit] =
        breakpoints.update(bp => bp.copy(value = bp.value + args))
    }

  /** Translate parse events to updated Daffodil state. */
  def fromParse(
      frameIds: DAPodil.Frame.Id.Next
  ): Stream[IO, Parse.Event] => Stream[IO, DAPodil.Data] =
    events =>
      events.evalScan(DAPodil.Data.empty) {
        case (_, Parse.Event.Init(_)) => IO.pure(DAPodil.Data.empty)
        case (prev, startElement: Parse.Event.StartElement) =>
          frameIds.next.map(nextFrameId => prev.push(createFrame(startElement, nextFrameId)))
        case (prev, Parse.Event.EndElement(_)) => IO.pure(prev.pop)
        case (prev, _: Parse.Event.Fini.type)  => IO.pure(prev)
      }

  def createLocation(loc: SchemaFileLocation): DAPodil.Location =
    DAPodil.Location(
      DAPodil.Path(new URI(loc.uriString).getPath),
      DAPodil.Line(loc.lineNumber.map(_.toInt).getOrElse(0))
    )

  /** Transform Daffodil state to a DAP stack frame.
    *
    * @see https://microsoft.github.io/debug-adapter-protocol/specification#Types_StackFrame
    */
  def createFrame(startElement: Parse.Event.StartElement, id: DAPodil.Frame.Id): DAPodil.Frame =
    DAPodil.Frame(
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

  sealed trait Control {
    def await(): IO[Unit]

    def continue(): IO[Unit]
    def step(): IO[Unit]
    def pause(): IO[Unit]
  }

  object Control {

    sealed trait State
    case object Running extends State
    case class Stopped(whenContinued: Deferred[IO, Unit]) extends State

    def initial(): IO[Control] =
      for {
        whenContinued <- Deferred[IO, Unit]
        state <- Ref[IO].of[State](Stopped(whenContinued))
      } yield new Control {
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
