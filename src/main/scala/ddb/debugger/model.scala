package ddb.debugger

import org.apache.daffodil.infoset._
import org.apache.daffodil.processors.parsers.PState
import zio.ZIO

object model {
  object Step {
    val only: Step = Step(Seq.empty)
    val default: Step = Step(Seq(ShowInfoset))
  }
  case class Step(and: Seq[Command[_ <: Event]]) extends Command[StepEvent] {
    override def run(state: PState): ZIO[Any, Throwable, StepEvent] = ZIO.foreach(and)(_.run(state)).map(StepComplete)
  }
  sealed trait StepEvent extends Event
  case class StepComplete(events: Seq[Event]) extends MultiEvent with StepEvent

  case object ShowInfoset extends Command[InfosetEvent] {
    def run(state: PState): ZIO[Any, Throwable, InfosetEvent] =
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
  trait InfosetEvent extends Event
  case object NoInfoset extends InfosetEvent
  case class ViewInfosetEvent(xml: String) extends InfosetEvent
}
