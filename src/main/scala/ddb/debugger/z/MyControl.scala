package ddb.debugger.z

import ddb.debugger.api.{ControlProvider, MultiEvent, Step}
import ddb.debugger.z.cmdq.CmdQueue
import ddb.debugger.z.ebus.Eventbus
import scalafx.application.Platform.runLater
import scalafx.scene.control.Slider
import zio.{IO, Ref, ZIO}

/**
  * represents a control, like a "step into" button that would action and then wait for event to enable again
  */
case class MyControl() {
  def step() = CmdQueue.get().flatMap(_.offer(Step.default))
  def run() = ZIO.unit
}

case class MySliderControl(history: Ref[List[MultiEvent]])(implicit rt: DebuggerRuntime) extends ControlProvider {
  def run() =
    Eventbus
      .sub()
      .flatMap(_.foreach {
        case e: MultiEvent =>
          // todo;; updating history should be elsewhere
          history.getAndUpdate(_ :+ e).map(_.size + 1).flatMap { cnt =>
            IO {
              runLater {
                control.max = cnt
                control.value = cnt
              }
            }
          }
        case _ => ZIO.unit
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

    value.onChange((_, _, v) =>
      // dont fire unless change is from human interaction (mouse pressed)
      if (pressed.get()) {
        rt.unsafeRunAsync_(
          for {
            // there is a little slop at the end of the slider, just guarding with option for now
            v <- history.get.map(l => l.lift(v.intValue))
            _ <- ZIO.fromOption(v).map(_.events()).flatMap(Eventbus.pubAll)
          } yield ()
        )
      }
    )
  }
}
