package ddb.debugger.dapodil

import cats.Show
import cats.effect._
import cats.effect.std._
import cats.syntax.all._
import com.microsoft.java.debug.core.protocol._
import com.microsoft.java.debug.core.protocol.Requests.EvaluateArguments
import fs2._
import fs2.concurrent.Signal
import fs2.io.file.Files
import java.io._
import java.net.URI
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.infoset._
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.processors._
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import org.apache.daffodil.util.Misc
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait Parse {

  /** Run the parse, returning the bytes of the final infoset. */
  def run(): Stream[IO, Byte]

  def close(): IO[Unit]
}

/** A running Daffodil parse. */
object Parse {
  implicit val logger: Logger[IO] = Slf4jLogger.getLogger

  def apply(
      schema: URI,
      data: InputStream,
      debugger: Debugger
  ): IO[Parse] =
    for {
      dp <- Compiler().compile(schema).map(p => p.withDebugger(debugger).withDebugging(true))
      done <- Ref[IO].of(false)
      pleaseStop <- Deferred[IO, Unit]
    } yield new Parse {
      def run(): Stream[IO, Byte] =
        fs2.io
          .readOutputStream(4096) { os =>
            val stopper =
              pleaseStop.get *> IO.canceled // will cancel the concurrent parse effect

            val parse =
              IO.interruptible(true) {
                  dp.parse(new InputSourceDataInputStream(data), new XMLTextInfosetOutputter(os, true)) // WARNING: parse doesn't close the OutputStream, so closed below
                }
                .guaranteeCase(outcome => Logger[IO].debug(s"parse finished: $outcome"))
                .void

            stopper &> parse.guarantee(IO(os.close) *> done.set(true))
          }

      def close(): IO[Unit] =
        done.get.flatMap {
          case false => Logger[IO].debug("interrupting parse") *> pleaseStop.complete(()).void
          case true  => Logger[IO].debug("parse done, no interruption") *> IO.unit
        }
    }

  class Debugee(
      outputData: Signal[IO, DAPodil.Data],
      outputState: Stream[IO, DAPodil.Debugee.State],
      stateSink: QueueSink[IO, Option[DAPodil.Debugee.State]],
      outputEvents: Stream[IO, Events.OutputEvent],
      breakpoints: Breakpoints,
      control: Control
  ) extends DAPodil.Debugee {
    def data(): Signal[IO, DAPodil.Data] =
      outputData

    def state(): Stream[IO, DAPodil.Debugee.State] =
      outputState

    def outputs(): Stream[IO, Events.OutputEvent] =
      outputEvents

    def step(): IO[Unit] =
      control.step() *> stateSink.offer(
        Some(DAPodil.Debugee.State.Stopped(DAPodil.Debugee.State.Stopped.Reason.Step))
      )

    def continue(): IO[Unit] =
      control.continue() *> stateSink.offer(Some(DAPodil.Debugee.State.Running))

    def pause(): IO[Unit] =
      control.pause() *> stateSink.offer(
        Some(DAPodil.Debugee.State.Stopped(DAPodil.Debugee.State.Stopped.Reason.Pause))
      )

    def setBreakpoints(args: (DAPodil.Path, List[DAPodil.Line])): IO[Unit] =
      breakpoints.setBreakpoints(args)

    def eval(args: EvaluateArguments): IO[Option[Types.Variable]] =
      args.expression match {
        case name => // check all variables in the given frame
          data().get.map { data =>
            for {
              frame <- data.stack.findFrame(DAPodil.Frame.Id(args.frameId))
              variable <- frame.scopes.collectFirstSome { scope =>
                scope.variables.values.toList.collectFirstSome(_.find(_.name == name))
              }
            } yield variable
          }
      }
  }

  def debugee(
      schema: URI,
      in: InputStream,
      infosetOutput: DAPodil.LaunchArgs.InfosetOutput
  ): Resource[IO, DAPodil.Debugee] =
    for {
      data <- Resource.eval(Queue.bounded[IO, Option[DAPodil.Data]](10))
      state <- Resource.eval(Queue.bounded[IO, Option[DAPodil.Debugee.State]](10))
      outputs <- Resource.eval(Queue.bounded[IO, Option[Events.OutputEvent]](10))
      breakpoints <- Resource.eval(Breakpoints())
      control <- Resource.eval(Control.initial())

      latestData <- Stream.fromQueueNoneTerminated(data).holdResource(DAPodil.Data.empty)
      debugee = new Debugee(
        latestData,
        Stream.fromQueueNoneTerminated(state),
        state,
        Stream.fromQueueNoneTerminated(outputs),
        breakpoints,
        control
      )

      events <- Resource.eval(Queue.bounded[IO, Option[Event]](10))
      debugger <- DaffodilDebugger.resource(state, outputs, events, breakpoints, control)
      parse <- Resource.eval(Parse(schema, in, debugger))

      infosetWriting = infosetOutput match {
        case DAPodil.LaunchArgs.InfosetOutput.None =>
          parse.run().drain
        case DAPodil.LaunchArgs.InfosetOutput.Console =>
          parse
            .run()
            .through(text.utf8Decode)
            .foldMonoid
            .evalTap(_ => Logger[IO].debug("done collecting infoset XML output"))
            .map(infosetXML => Events.OutputEvent.createConsoleOutput(infosetXML))
            .enqueueNoneTerminated(outputs)
        case DAPodil.LaunchArgs.InfosetOutput.File(path) =>
          parse.run().through(Files[IO].writeAll(path))
      }

      nextFrameId <- Resource.eval(Next.int.map(_.map(DAPodil.Frame.Id.apply)))
      nextScopeId <- Resource.eval(Next.int.map(_.map(DAPodil.VariablesReference.apply)))

      deliverParseData = Stream
        .fromQueueNoneTerminated(events)
        .through(fromParse(nextFrameId, nextScopeId))
        .enqueueNoneTerminated(data)

      _ <- Stream
        .emit(debugee)
        .concurrently(Stream(infosetWriting, deliverParseData).parJoinUnbounded)
        .compile
        .resource
        .lastOrError

      _ <- Resource.onFinalize(Logger[IO].debug("signalling a stop") *> parse.close())
    } yield debugee

  /** Translate parse events to updated Daffodil state. */
  def fromParse(
      frameIds: Next[DAPodil.Frame.Id],
      variableRefs: Next[DAPodil.VariablesReference]
  ): Stream[IO, Parse.Event] => Stream[IO, DAPodil.Data] =
    events =>
      events.evalScan(DAPodil.Data.empty) {
        case (_, Parse.Event.Init(_)) => IO.pure(DAPodil.Data.empty)
        case (prev, startElement: Parse.Event.StartElement) =>
          createFrame(startElement, frameIds, variableRefs).map(prev.push)
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
      variableRefs: Next[DAPodil.VariablesReference]
  ): IO[DAPodil.Frame] =
    for {
      ids <- (frameIds.next, variableRefs.next, variableRefs.next, variableRefs.next).tupled
      (frameId, parseScopeId, schemaScopeId, dataScopeId) = ids

      stackFrame = new Types.StackFrame(
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

      schemaScope <- schemaScope(schemaScopeId, startElement.state, variableRefs)
    } yield DAPodil.Frame(
      frameId,
      stackFrame,
      List(
        parseScope(parseScopeId, startElement.state),
        schemaScope,
        dataScope(dataScopeId, startElement.state)
      )
    )

  def parseScope(ref: DAPodil.VariablesReference, state: StateForDebugger): DAPodil.Frame.Scope = {
    val hidden = state.withinHiddenNest
    val childIndex = if (state.childPos != -1) Some(state.childPos) else None
    val groupIndex = if (state.groupPos != -1) Some(state.groupPos) else None
    val occursIndex = if (state.arrayPos != -1) Some(state.arrayPos) else None
    val foundDelimiter = for {
      dpr <- state.delimitedParseResult.toScalaOption
      dv <- dpr.matchedDelimiterValue.toScalaOption
    } yield Misc.remapStringToVisibleGlyphs(dv)
    val foundField = for {
      dpr <- state.delimitedParseResult.toScalaOption
      f <- dpr.field.toScalaOption
    } yield Misc.remapStringToVisibleGlyphs(f)

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

    DAPodil.Frame.Scope(
      "Parse",
      ref,
      Map(ref -> parseVariables)
    )
  }

  def schemaScope(
      scopeRef: DAPodil.VariablesReference,
      state: StateForDebugger,
      refs: Next[DAPodil.VariablesReference]
  ): IO[DAPodil.Frame.Scope] =
    state.variableMap.qnames.toList
      .groupBy(_.namespace) // TODO: handle NoNamespace or UnspecifiedNamespace as top-level?
      .toList
      .flatTraverse {
        case (ns, vs) =>
          // every namespace is a DAP variable in the current scope, and links to its set of Daffodil-as-DAP variables
          refs.next.map { ref =>
            List(scopeRef -> List(new Types.Variable(ns.toString(), "", null, ref.value, null))) ++
              List(
                ref -> vs
                  .sortBy(_.toPrettyString)
                  .fproduct(state.variableMap.find)
                  .map {
                    case (name, value) =>
                      new Types.Variable(
                        name.toQNameString,
                        value
                          .flatMap(v => Option(v.value.value).map(_.toString) orElse Some("null"))
                          .getOrElse("???"),
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
              )
          }
      }
      .map { refVars =>
        val sv = refVars.foldMap(Map(_)) // combine values of map to accumulate namespaces
        DAPodil.Frame.Scope(
          "Schema",
          scopeRef,
          sv
        )
      }

  def dataScope(ref: DAPodil.VariablesReference, state: StateForDebugger): DAPodil.Frame.Scope = {
    val bytePos1b = state.currentLocation.bytePos1b
    val dataVariables: List[Types.Variable] =
      List(new Types.Variable("bytePos1b", bytePos1b.toString, "number", 0, null))

    DAPodil.Frame.Scope(
      "Data",
      ref,
      Map(ref -> dataVariables)
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

  trait Breakpoints {
    def setBreakpoints(args: (DAPodil.Path, List[DAPodil.Line])): IO[Unit]
    def shouldBreak(location: DAPodil.Location): IO[Boolean]
  }

  object Breakpoints {
    def apply(): IO[Breakpoints] =
      for {
        breakpoints <- Ref[IO].of(DAPodil.Breakpoints.empty)
      } yield new Breakpoints {
        def setBreakpoints(args: (DAPodil.Path, List[DAPodil.Line])): IO[Unit] =
          breakpoints.update(bp => bp.copy(value = bp.value + args))

        def shouldBreak(location: DAPodil.Location): IO[Boolean] =
          for {
            bp <- breakpoints.get
          } yield bp.contains(location)
      }
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

  class DaffodilDebugger(
      dispatcher: Dispatcher[IO],
      previousState: Ref[IO, Option[StateForDebugger]],
      state: QueueSink[IO, Option[DAPodil.Debugee.State]],
      sendOutput: QueueSink[IO, Option[Events.OutputEvent]],
      breakpoints: Breakpoints,
      control: Control,
      events: QueueSink[IO, Option[Event]]
  ) extends Debugger {

    override def init(pstate: PState, processor: Parser): Unit =
      dispatcher.unsafeRunSync {
        events.offer(Some(Event.Init(pstate.copyStateForDebugger))) *>
          previousState.set(Some(pstate.copyStateForDebugger))
      }

    override def fini(processor: Parser): Unit =
      dispatcher.unsafeRunSync {
        events.offer(Some(Event.Fini)) *> events.offer(None) *> // no more events after this
          state.offer(None) // no more states == the parse is terminated
      }

    override def startElement(pstate: PState, processor: Parser): Unit =
      dispatcher.unsafeRunSync {
        val push = Event.StartElement(
          pstate.copyStateForDebugger,
          pstate.currentNode.toScalaOption.map(_.name),
          pstate.schemaFileLocation
        )
        logger.debug("pre-offer") *>
          events.offer(Some(push)) *>
          logger.debug("pre-checkBreakpoints") *>
          checkBreakpoints(createLocation(pstate.schemaFileLocation)) *>
          logger.debug("pre-control await") *>
          // may block until external control says to unblock, for stepping behavior
          control
            .await()
            .ifM(sendOutput(pstate), IO.unit) *> // if we are stepping, send output events, otherwise no-op
          logger.debug("post-control await") *>
          previousState.set(Some(pstate.copyStateForDebugger))
      }

    def sendOutput(state: PState): IO[Unit] =
      for {
        prev <- previousState.get
        _ <- prev.traverse { prestate =>
          val dataLoc = prestate.currentLocation.asInstanceOf[DataLoc]
          val lines = dataLoc.dump(None, prestate.currentLocation, state)

          sendOutput.offer(Some(Events.OutputEvent.createConsoleOutput(lines + "\n")))
        }
      } yield ()

    def checkBreakpoints(location: DAPodil.Location): IO[Unit] =
      breakpoints
        .shouldBreak(location)
        .ifM(
          control
            .pause() *> state.offer(Some(DAPodil.Debugee.State.Stopped(DAPodil.Debugee.State.Stopped.Reason.Pause))),
          IO.unit
        )

    override def endElement(pstate: PState, processor: Parser): Unit =
      dispatcher.unsafeRunSync {
        events.offer(Some(Event.EndElement(pstate.copyStateForDebugger)))
      }
  }

  object DaffodilDebugger {
    def resource(
        state: QueueSink[IO, Option[DAPodil.Debugee.State]],
        outputs: QueueSink[IO, Option[Events.OutputEvent]],
        events: QueueSink[IO, Option[Event]],
        breakpoints: Breakpoints,
        control: Control
    ): Resource[IO, DaffodilDebugger] =
      for {
        dispatcher <- Dispatcher[IO]
        previousState <- Resource.eval(Ref[IO].of[Option[StateForDebugger]](None))
      } yield new DaffodilDebugger(
        dispatcher,
        previousState,
        state,
        outputs,
        breakpoints,
        control,
        events
      )
  }
}
