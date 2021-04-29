package ddb.debugger.z

import ddb.debugger.api.{ControlProvider, Step}
import ddb.debugger.z.cmdq.CmdQueue
import ddb.debugger.z.ebus.Eventbus
import ddb.debugger.z.history.History
import scalafx.application.Platform.runLater
import scalafx.scene.control.Slider
import zio._

/**
  * represents a control, like a "step into" button that would action and then wait for event to enable again
  */
case class MyControl() {
  def step() = CmdQueue.get().flatMap(_.offer(Step.default))
  def run() = ZIO.unit
}

case class MySliderControl()(implicit rt: DebuggerRuntime) extends ControlProvider {
  def run() =
    History
      .sub()
      .flatMap(_.foreach {
        case (_, idx) =>
          IO {
            runLater {
              control.max = idx.toDouble
              control.value = idx.toDouble
            }
          }
      })

  lazy val control: Slider = new Slider {
    layoutX = 300
    layoutY = 390
    prefWidth = 350
    prefHeight = 10

    showTickMarks = true
    showTickLabels = true
    minorTickCount = 1
    min = 1
    max = 1
    snapToTicks = true
    blockIncrement = 1.0

    value.onChange((_, prev, curr) =>
      // dont fire unless change is from human interaction (mouse pressed)
      if (pressed.get()) {
        val current = curr.intValue()
        // the slider is double based, which results in many false events being fired
        // so dont fire unless the movement is enough to produce different int values
        if (current != prev.intValue()) {
          runLater {
            rt.unsafeRunSync(
              History
                .all()
                .map(l => l.lift(current)) // todo;; indexing slop, just guard with option for now
                .flatMap(v => ZIO.fromOption(v).map(_.events).flatMap(Eventbus.pubAll))
            )
          }
        }
      }
    )
  }
}
