package ddb.debugger.z

import ddb.debugger.api.Step
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.TextArea
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import zio.IO

object gui {
  def run(cq: CProducer, infosetCtl: ControlProvider[TextArea]) =
    for {
      _ <- IO {
        val jfxApp = new JFXApp {
          override def stopApp(): Unit = System.exit(0)

          stage = new JFXApp.PrimaryStage {
            title.value = "Hello Stage"
            width = 600
            height = 450
            scene = new Scene {
              fill = LightGreen
              val button = new Rectangle {
                x = 25
                y = 240
                width = 100
                height = 100
                fill <== when(hover) choose Green otherwise Red
                onMouseClicked = _ =>
                  zio.Runtime.default.unsafeRunAsync_(
                    cq.offer(Step.default)
                  )
              }

              content = List(button, infosetCtl.control)
            }
          }
        }
        jfxApp.main(Array.empty)
      }
    } yield ()
}
