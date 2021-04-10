package ddb.debugger.z

import ddb.debugger.api.{Command, Step, StepEvent}
import zio.console.putStrLn
import zio.stream.Sink
import zio.{Hub, ZIO}

/**
  * represents a control, like a "step into" button that would action and then wait for event to enable again
  */
case class MyControl(cs: CProducer) {
  def step() = cs.offer(Step.default)

  def run(stream: EStream) = {
    stream.foreach {
        case _: StepEvent =>
          for {
            // todo;; autostepping for demo
            _ <- cs.offer(Step.default)
            _ <- putStrLn("stepping")
          } yield ()
        case _ =>
          ZIO.unit
      }
  }
}
