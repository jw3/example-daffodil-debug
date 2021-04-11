package ddb.debugger.z

import ddb.debugger.api.Step
import zio.ZIO

/**
  * represents a control, like a "step into" button that would action and then wait for event to enable again
  */
case class MyControl(cs: CProducer) {
  def step() = cs.offer(Step.default)
  def run(stream: EStream) = ZIO.unit
}
