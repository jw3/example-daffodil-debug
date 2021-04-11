package ddb.debugger.z

import ddb.debugger.api.{BitPosEvent, ViewInfosetEvent}
import zio._
import zio.console.putStrLn

/**
  * represents displays such as GUI panels that would receive events, likely filtering from the overall
  * stream of events and do something with them without responding with commands
  */
case class MyInfoSetDisplay() {
  def run(es: EStream) = es.foreach {
    case ViewInfosetEvent(xml) => putStrLn(s"[MyInfosSetDisplay] $xml")
    case _                     => ZIO.unit
  }
}

case class MyBitPosDisplay() {
  def run(es: EStream) = es.foreach {
    case e @ BitPosEvent(_) => putStrLn(s"[MyBitPosDisplay] $e")
    case _                  => ZIO.unit
  }
}
