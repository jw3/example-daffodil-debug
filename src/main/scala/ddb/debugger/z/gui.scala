package ddb.debugger.z

import ddb.debugger.api.{ControlProvider, Step}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import zio.IO

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
              val button = new Button {
                id = "debug-step"
                layoutX = 0
                layoutY = 350
                prefWidth = 48
                prefHeight = 48
                graphic = new ImageView(new Image(this, s"/icons/debug-step-into.png"))
                tooltip = "Step"
                onMouseClicked = _ =>
                  zio.Runtime.default.unsafeRunAsync_(
                    cq.offer(Step.default)
                  )
              }

              content = myctls.map(_.control) :+ button
            }
          }
        }
        jfxApp.main(Array.empty)
      }
    } yield ()
}
