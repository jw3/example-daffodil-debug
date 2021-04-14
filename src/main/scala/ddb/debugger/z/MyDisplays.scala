package ddb.debugger.z

import com.github.difflib.DiffUtils
import ddb.debugger.api.{BitPosEvent, ViewInfosetEvent}
import scalafx.scene.control.{Control, TextArea}
import zio._
import zio.console.putStrLn

import scala.collection.JavaConverters._

/**
  * represents displays such as GUI panels that would receive events, likely filtering from the overall
  * stream of events and do something with them without responding with commands
  */
trait ControlProvider[C <: Control] {
  def control: C
}

case class MyInfoSetDisplay() extends ControlProvider[TextArea] {
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

case class MyBitPosDisplay() {
  def run(es: EStream) = es.foreach {
    case e @ BitPosEvent(_) => putStrLn(s"[MyBitPosDisplay] $e")
    case _                  => ZIO.unit
  }
}

// a stateful consumer
case class MyDiffingInfoSetDisplay(prevRef: Ref[String]) {
  def run(es: EStream) = es.foreach {
    case ViewInfosetEvent(xml) =>
      for {
        prev <- prevRef.getAndSet(xml)
        diff <- IO {
          DiffUtils.diff(prev.linesIterator.toSeq.asJava, xml.linesIterator.toSeq.asJava).toString
        }
        _ <- putStrLn(s"[MyInfosSetDiffDisplay]\n$diff")
      } yield ()
    case _ => ZIO.unit
  }
}
