package ddb.debugger

package object event {
  sealed trait StepEvent extends Event
  case class StepComplete(events: Seq[Event]) extends MultiEvent with StepEvent

  trait InfosetEvent extends Event
  case object NoInfoset extends InfosetEvent
  case class ViewInfosetEvent(xml: String) extends InfosetEvent
}
