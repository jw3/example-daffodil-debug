package ddb.debugger.dapodil

import cats.Show
import cats.effect._
import cats.effect.std._
import cats.syntax.all._
import com.microsoft.java.debug.core.protocol._
import fs2._
import fs2.concurrent.Signal
import java.io.InputStream
import java.net.URI
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.processors._
import org.apache.daffodil.sapi.infoset.NullInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import org.apache.daffodil.util.Misc
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import com.microsoft.java.debug.core.protocol.Requests.EvaluateArguments
import org.apache.daffodil.infoset.DIDocument
import org.apache.daffodil.infoset.InfosetElement
import org.apache.daffodil.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.infoset.InfosetWalker
import org.apache.daffodil.infoset.DIElement

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
      // TODO: explore fs2.Channel as alternative to Queue
      parseEvents <- Resource.eval(Queue.bounded[IO, Option[Event]](10))
      parseState <- Resource.eval(Queue.bounded[IO, Option[DAPodil.Debugee.State]](10))
      outputEvents <- Resource.eval(Queue.bounded[IO, Option[Events.OutputEvent]](10))
      previousState <- Resource.eval(Ref[IO].of[Option[StateForDebugger]](None))
      control <- Resource.eval(Control.initial())
      breakpoints <- Resource.eval(Ref[IO].of(DAPodil.Breakpoints.empty))
      infoset <- Resource.eval(Ref[IO].of[Option[String]](None)) // TODO: infoset isn't global, it's per-stack-frame(?)
      debugger = new Daffodil(
        dispatcher,
        parseEvents,
        previousState,
        parseState,
        control,
        outputEvents,
        breakpoints,
        infoset
      )
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
            _ <- parseState.tryOffer(None)
            _ <- outputEvents.tryOffer(None)
            offered <- parseEvents.tryOffer(None)
            _ <- if (offered) IO.unit else Logger[IO].warn("producer couldn't end the queue when shutting down")
          } yield ()
        }
        .background
        .onFinalizeCase {
          case ec @ Resource.ExitCase.Errored(t) => Logger[IO].error(t)(s"parse: $ec")
          case ec                                => Logger[IO].debug(s"parse: $ec")
        }

      nextFrameId <- Resource.eval(Next.int.map(_.map(DAPodil.Frame.Id.apply)))
      nextScopeId <- Resource.eval(Next.int.map(_.map(DAPodil.Frame.Scope.VariablesReference.apply)))

      debugeeData <- Stream
        .fromQueueNoneTerminated(parseEvents)
        .through(fromParse(nextFrameId, nextScopeId))
        .holdResource(DAPodil.Data.empty)
    } yield new DAPodil.Debugee {
      def data(): Signal[IO, DAPodil.Data] =
        debugeeData

      def state(): Stream[IO, DAPodil.Debugee.State] =
        Stream.fromQueueNoneTerminated(parseState)

      def outputs(): Stream[IO, Events.OutputEvent] =
        Stream.fromQueueNoneTerminated(outputEvents)

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

      def eval(args: EvaluateArguments): IO[Option[Types.Variable]] =
        args.expression match {
          case "infoset" =>
            infoset.get.map {
              case Some(i) => Some(new Types.Variable("infoset", i))
              case None    => None
            }
          case name => // check all variables in the given frame
            data().get.map { data =>
              for {
                frame <- data.stack.findFrame(DAPodil.Frame.Id(args.frameId))
                variable <- frame.scopes.collectFirstSome { scope =>
                  scope.variables.find(_.name == name)
                }
              } yield variable
            }
        }
    }

  /** Translate parse events to updated Daffodil state. */
  def fromParse(
      frameIds: Next[DAPodil.Frame.Id],
      scopeIds: Next[DAPodil.Frame.Scope.VariablesReference]
  ): Stream[IO, Parse.Event] => Stream[IO, DAPodil.Data] =
    events =>
      events.evalScan(DAPodil.Data.empty) {
        case (_, Parse.Event.Init(_)) => IO.pure(DAPodil.Data.empty)
        case (prev, startElement: Parse.Event.StartElement) =>
          createFrame(startElement, frameIds, scopeIds).map(prev.push)
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
  def createFrame(
      startElement: Parse.Event.StartElement,
      frameIds: Next[DAPodil.Frame.Id],
      scopeIds: Next[DAPodil.Frame.Scope.VariablesReference]
  ): IO[DAPodil.Frame] =
    (frameIds.next, scopeIds.next, scopeIds.next, scopeIds.next).mapN {
      (
          frameId,
          parseScopeId,
          schemaScopeId,
          dataScopeId
      ) =>
        val stackFrame = new Types.StackFrame(
          /* It must be unique across all threads.
           * This id can be used to retrieve the scopes of the frame with the
           * 'scopesRequest' or to restart the execution of a stackframe.
           */
          frameId.value,
          startElement.name.getOrElse("???"),
          /* If sourceReference > 0 the contents of the source must be retrieved through
           * the SourceRequest (even if a path is specified). */
          new Types.Source(startElement.schemaLocation.uriString, 0),
          startElement.schemaLocation.lineNumber
            .map(_.toInt)
            .getOrElse(1), // line numbers start at 1 according to InitializeRequest
          0 // column numbers start at 1 according to InitializeRequest, but set to 0 to ignore it; column calculation by Daffodil uses 1 tab = 2 spaces(?), but breakpoints use 1 character per tab
        )

        val bytePos1b = startElement.state.currentLocation.bytePos1b
        val hidden = startElement.state.withinHiddenNest
        val childIndex = if (startElement.state.childPos != -1) Some(startElement.state.childPos) else None
        val groupIndex = if (startElement.state.groupPos != -1) Some(startElement.state.groupPos) else None
        val occursIndex = if (startElement.state.arrayPos != -1) Some(startElement.state.arrayPos) else None
        val foundDelimiter = for {
          dpr <- startElement.state.delimitedParseResult.toScalaOption
          dv <- dpr.matchedDelimiterValue.toScalaOption
        } yield Misc.remapStringToVisibleGlyphs(dv)
        val foundField = for {
          dpr <- startElement.state.delimitedParseResult.toScalaOption
          f <- dpr.field.toScalaOption
        } yield Misc.remapStringToVisibleGlyphs(f)

        val schemaVariables: List[Types.Variable] =
          startElement.state.variableMap.qnames
            .sortBy(_.toPrettyString)
            .toList
            .fproduct(startElement.state.variableMap.find)
            .map {
              case (name, value) =>
                new Types.Variable(
                  name.toPrettyString,
                  value.map(_.value.value.toString).getOrElse("???"),
                  value
                    .map(_.state match {
                      case VariableDefined      => "default"
                      case VariableRead         => "read"
                      case VariableSet          => "set"
                      case VariableUndefined    => "undefined"
                      case VariableBeingDefined => "being defined"
                      case VariableInProcess    => "in process"
                    })
                    .getOrElse("???"),
                  0,
                  null
                )
            }

        val parseVariables: List[Types.Variable] =
          (List(
            new Types.Variable("hidden", hidden.toString, "bool", 0, null)
          ) ++ childIndex.map(ci => new Types.Variable("childIndex", ci.toString)).toList
            ++ groupIndex
              .map(gi => new Types.Variable("groupIndex", gi.toString))
              .toList
            ++ occursIndex
              .map(oi => new Types.Variable("occursIndex", oi.toString))
              .toList
            ++ foundDelimiter.map(fd => new Types.Variable("foundDelimiter", fd)).toList
            ++ foundField.map(ff => new Types.Variable("foundField", ff)).toList)
            .sortBy(_.name)

        val dataVariables: List[Types.Variable] =
          List(new Types.Variable("bytePos1b", bytePos1b.toString, "number", 0, null))

        DAPodil.Frame(
          frameId,
          stackFrame,
          List(
            DAPodil.Frame.Scope(
              "Parse",
              parseScopeId,
              parseVariables
            ),
            DAPodil.Frame.Scope(
              "Schema",
              schemaScopeId,
              schemaVariables
            ),
            DAPodil.Frame.Scope(
              "Data",
              dataScopeId,
              dataVariables
            )
          )
        )
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

  sealed trait Control {
    def await(): IO[Boolean]

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
        def await(): IO[Boolean] =
          state.get.flatMap {
            case Running                => IO.pure(false)
            case Stopped(whenContinued) => whenContinued.get.as(true)
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

  class Daffodil(
      dispatcher: Dispatcher[IO],
      parseEvents: Queue[IO, Option[Event]],
      previousState: Ref[IO, Option[StateForDebugger]],
      parseState: Queue[IO, Option[DAPodil.Debugee.State]],
      control: Control,
      outputEvents: Queue[IO, Option[Events.OutputEvent]],
      breakpoints: Ref[IO, DAPodil.Breakpoints],
      infoset: Ref[IO, Option[String]]
  ) extends Debugger {

    override def init(pstate: PState, processor: Parser): Unit =
      dispatcher.unsafeRunSync {
        parseEvents.offer(Some(Event.Init(pstate.copyStateForDebugger))) *>
          previousState.set(Some(pstate.copyStateForDebugger))
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
          // may block until external control says to unblock, for stepping behavior
          control
            .await()
            .ifM(sendOutput(pstate), IO.unit) *> // if we are stepping, send output events, otherwise no-op
          logger.debug("post-control await") *>
          previousState.set(Some(pstate.copyStateForDebugger)) *>
          infoset.set(extractInfoset(pstate))
      }

    def sendOutput(state: PState): IO[Unit] =
      for {
        prev <- previousState.get
        _ <- prev.traverse { prestate =>
          val dataLoc = prestate.currentLocation.asInstanceOf[DataLoc]
          val lines = dataLoc.dump(None, prestate.currentLocation, state)

          outputEvents.offer(Some(Events.OutputEvent.createConsoleOutput(lines + "\n")))
        }
      } yield ()

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

    def extractInfoset(pstate: PState): Option[String] =
      if (pstate.hasInfoset) {
        var parentSteps = Int.MaxValue
        var node = pstate.infoset

        while (parentSteps > 0 && node.diParent != null) {
          node = node.diParent
          parentSteps -= 1
        }

        node match {
          case d: DIDocument if d.contents.size == 0 => None
          case _                                     => Some(infosetToString(node))
        }
      } else None

    private def infosetToString(ie: InfosetElement): String = {
      val bos = new java.io.ByteArrayOutputStream()
      val xml = new XMLTextInfosetOutputter(bos, true)
      val iw = InfosetWalker(
        ie.asInstanceOf[DIElement],
        xml,
        walkHidden = true,
        ignoreBlocks = true,
        releaseUnneededInfoset = false
      )
      iw.walk(lastWalk = true)
      bos.toString("UTF-8")
    }

  }
}
