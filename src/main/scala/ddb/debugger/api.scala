package ddb.debugger

import org.apache.daffodil.infoset._
import org.apache.daffodil.processors._
import org.apache.daffodil.processors.parsers.PState
import scalafx.scene.control.Control
import zio.{IO, Task, ZIO}

object api {
  // gui interaction and displays
  trait ControlProvider {
    def control: Control
  }

  // a command applied to the state produces an event
  // Command(state) => Event
  trait Command[E <: Event] {
    def run(state: PState, processor: Processor): Task[E]
  }
  trait Event
  trait MultiEvent extends Event {
    def events(): Seq[Event]
  }

  // --------------------------------------
  object Step {
    val only: Step = Step(Seq.empty)
    val default: Step = Step(Seq(BitPosition, ShowInfoset, ShowPath, ShowVariables))
  }
  case class Step(and: Seq[Command[_ <: Event]]) extends Command[StepEvent] {
    override def run(state: PState, processor: Processor): Task[StepEvent] =
      ZIO.foreach(and)(_.run(state, processor)).map(StepComplete)
  }

  // --------------------------------------

  case object BitPosition extends Command[BitPosEvent] {
    def run(state: PState, processor: Processor): Task[BitPosEvent] =
      ZIO(state.bitPos0b).map(BitPosEvent)
  }
  case class BitPosEvent(pos: Long) extends Event

  // ---------------------------------------

  case object ShowPath extends Command[PathEvent] {
    def run(state: PState, processor: Processor): Task[PathEvent] =
      ZIO(processor.context.path).map(PathEvent)
  }
  case class PathEvent(path: String) extends Event

  // ---------------------------------------

  case object ShowVariables extends Command[VariablesEvent] {
    def run(state: PState, processor: Processor): Task[VariablesEvent] =
      IO {
        val vmap = state.variableMap
        val txt = vmap.qnames.sortBy { _.toPrettyString }.foldLeft(Seq.empty[String]) { (r, v) =>
          val instance = vmap.find(v).get
          val debugVal = variableInstanceToDebugString(instance)
          r :+ "  %s: %s".format(v.toPrettyString, debugVal)
        }
        VariablesEvent(txt.mkString("\n"))
      }

    def variableInstanceToDebugString(vinst: VariableInstance): String = {
      val state = vinst.state match {
        case VariableDefined      => "default"
        case VariableRead         => "read"
        case VariableSet          => "set"
        case VariableUndefined    => "undefined"
        case VariableBeingDefined => "being defined"
        case VariableInProcess    => "in process"
      }

      if (vinst.value.isEmpty) "(%s)".format(state)
      else "%s (%s)".format(vinst.value.value, state)
    }
  }
  case class VariablesEvent(path: String) extends Event

  // ---------------------------------------

  case object ShowInfoset extends Command[InfosetEvent] {
    def run(state: PState, processor: Processor): Task[InfosetEvent] =
      ZIO(state.infoset).map {
        case d: DIDocument if d.contents.isEmpty => NoInfoset
        case node                                => ViewInfosetEvent(infosetToString(node))
      }

    private def infosetToString(ie: InfosetElement): String = {
      val bos = new java.io.ByteArrayOutputStream()
      val xml = new XMLTextInfosetOutputter(bos, true)
      val iw = InfosetWalker(
        ie.asInstanceOf[DIElement],
        xml,
        walkHidden = false,
        ignoreBlocks = true,
        releaseUnneededInfoset = false
      )
      iw.walk(lastWalk = true)
      bos.toString("UTF-8")
    }
  }

  sealed trait StepEvent extends Event
  case class StepComplete(events: Seq[Event]) extends MultiEvent with StepEvent

  trait InfosetEvent extends Event
  case object NoInfoset extends InfosetEvent
  case class ViewInfosetEvent(xml: String) extends InfosetEvent
}
