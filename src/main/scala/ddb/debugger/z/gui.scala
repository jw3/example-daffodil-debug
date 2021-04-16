package ddb.debugger.z

import ddb.debugger.api.{ControlProvider, Step}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ToggleButton}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.paint.Color._
import zio.duration.durationInt
import zio.{IO, Schedule}

object gui {
  def run(cq: CProducer, myctls: ControlProvider*) =
    for {
      _ <- IO {
        val jfxApp = new JFXApp {
          override def stopApp(): Unit = System.exit(0)

          stage = new JFXApp.PrimaryStage {
            title.value = """Not "the" Daffodil Debugger"""
            width = 650
            height = 450
            scene = new Scene {
              fill = LightGreen
              val step = new Button {
                id = "debug-step"
                layoutX = 0
                layoutY = 350
                prefWidth = 48
                prefHeight = 48
                graphic = new ImageView(new Image(this, s"/icons/debug-step-into.png"))
                tooltip = "Step"
                onMouseClicked = _ => {
                  step10.selected = false
                  zio.Runtime.default.unsafeRunAsync_(
                    cq.offer(Step.default)
                  )
                }
              }

              val step10 = new ToggleButton {
                id = "debug-play"
                layoutX = 70
                layoutY = 350
                prefWidth = 48
                prefHeight = 48
                graphic = new ImageView(new Image(this, s"/icons/debug-run.png"))
                tooltip = "Auto Step"

                onAction = e => {
                  zio.Runtime.default.unsafeRunAsync_(
                    cq.offer(Step.default).delay(100.millis).repeatWhile(_ => selected.value)
                  )
                }
              }

              val jpg = new ImageView {
                layoutX = 200
                layoutY = 320

                image = new Image(s"/works.jpg", 96,  96, true, true)
              }

              content = myctls.map(_.control) :+ step :+ step10 :+ jpg
            }
          }
        }
        jfxApp.main(Array.empty)
      }
    } yield ()
}
