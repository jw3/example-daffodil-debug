package ddb.debugger.z

import com.github.difflib.DiffUtils
import ddb.debugger.api.{BitPosEvent, ControlProvider, ViewInfosetEvent}
import scalafx.scene.control.{TextArea, TextField}
import zio._

import scala.collection.JavaConverters._

/**
  * represents displays such as GUI panels that would receive events, likely filtering from the overall
  * stream of events and do something with them without responding with commands
  */

case class MyInfoSetDisplay() extends ControlProvider {
  lazy val control: TextArea = new TextArea {
    layoutX = 0
    layoutY = 0
    prefWidth = 300
    prefHeight = 300
  }

  def run(es: EStream) = es.foreach {
    case ViewInfosetEvent(xml) => IO { control.text = xml }
    case _                     => ZIO.unit
  }
}

case class MyBitPosDisplay(input: Array[Byte]) extends ControlProvider {
  lazy val control: TextField = new TextField {
    layoutX = 300
    layoutY = 0
    prefWidth = 350
    prefHeight = 25
  }

  def run(es: EStream) =
    es.foreach {
        case BitPosEvent(pos) =>
          IO {
            val bytePos = pos.toInt / 8
            val txt = input
              .slice(bytePos - 4, bytePos + 5)
              .foldLeft(s"[$pos] ")((r, b) => String.format(s"$r %02x", Byte.box(b)))
            control.text = txt
          }
        case _ => ZIO.unit
      }
      .mapError(_ => IO { control.text = "err" })
}

// a stateful consumer
case class MyDiffingInfoSetDisplay(prevRef: Ref[String]) extends ControlProvider {
  lazy val control: TextArea = new TextArea {
    layoutX = 300
    layoutY = 25
    prefWidth = 350
    prefHeight = 275
  }

  def run(es: EStream) = es.foreach {
    case ViewInfosetEvent(xml) =>
      for {
        prev <- prevRef.getAndSet(xml)
        diff <- IO {
          DiffUtils.diff(prev.linesIterator.toSeq.asJava, xml.linesIterator.toSeq.asJava).toString
        }
        _ <- IO { control.text = diff.replaceAll("""([\[\]])""", "$1\n") }
      } yield ()
    case _ => ZIO.unit
  }
}
