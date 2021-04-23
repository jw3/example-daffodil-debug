package ddb.debugger.z

import com.github.difflib.DiffUtils
import ddb.debugger.api._
import scalafx.application.Platform.runLater
import scalafx.scene.control.{TextArea, TextField}
import zio._

import scala.collection.JavaConverters._

/**
  * represents displays such as GUI panels that would receive events, likely filtering from the overall
  * stream of events and do something with them without responding with commands
  */
case class MyInfoSetDisplay() {
  lazy val control = new TextArea {
    layoutX = 0
    layoutY = 0
    prefWidth = 300
    prefHeight = 300
  }

  def run(es: EStream) = es.foreach {
    case ViewInfosetEvent(xml) => IO { runLater(control.text = xml) }
    case _                     => ZIO.unit
  }
}

case class MyBitPosDisplay(input: Array[Byte]) {
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
          runLater(control.text = txt)
        }
      case _ => ZIO.unit
    }
}

case class MyPathDisplay() {
  lazy val control: TextArea = new TextArea {
    layoutX = 300
    layoutY = 25
    prefWidth = 350
    prefHeight = 100
  }

  def run(es: EStream) =
    es.foreach {
      case PathEvent(path) =>
        IO {
          val txt = path.replaceAll("""::""", "::\n")
          runLater(control.text = txt)
        }
      case _ => ZIO.unit
    }
}

case class MyVariablesDisplay() {
  lazy val control: TextArea = new TextArea {
    layoutX = 300
    layoutY = 125
    prefWidth = 350
    prefHeight = 125
  }

  def run(es: EStream) =
    es.foreach {
      case VariablesEvent(txt) =>
        IO {
          runLater(control.text = txt)
        }
      case _ => ZIO.unit
    }
}

// a stateful consumer
case class MyDiffingInfoSetDisplay(prevRef: Ref[String]) {
  lazy val control: TextArea = new TextArea {
    layoutX = 300
    layoutY = 250
    prefWidth = 350
    prefHeight = 125
  }

  def run(es: EStream) = es.foreach {
    case ViewInfosetEvent(xml) =>
      for {
        prev <- prevRef.getAndSet(xml)
        diff <- IO {
          DiffUtils.diff(prev.linesIterator.toSeq.asJava, xml.linesIterator.toSeq.asJava).toString
        }
        _ <- IO {
          val txt = diff.replaceAll("""([\[\]])""", "$1\n")
          runLater(control.text = txt)
        }
      } yield ()
    case _ => ZIO.unit
  }
}
