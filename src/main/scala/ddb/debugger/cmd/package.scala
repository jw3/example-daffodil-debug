package ddb.debugger

import ddb.debugger.cmd.infoset.ShowInfoset
import ddb.debugger.event.{StepComplete, StepEvent}
import org.apache.daffodil.processors.parsers.PState
import zio.ZIO

package object cmd {
  object Step {
    val only: Step = Step(Seq.empty)
    val default: Step = Step(Seq(ShowInfoset))
  }
  case class Step(and: Seq[Command[_ <: Event]]) extends Command[StepEvent] {
    override def run(state: PState): ZIO[Any, Throwable, StepEvent] = ZIO.foreach(and)(_.run(state)).map(StepComplete)
  }
}
