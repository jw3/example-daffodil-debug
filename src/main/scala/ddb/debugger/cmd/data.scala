package ddb.debugger.cmd

import ddb.debugger.{Command, Event}
import org.apache.daffodil.processors.parsers.PState
import zio.ZIO

object data {
  case object BitPosition extends Command[BitPosEvent] {
    def run(state: PState): ZIO[Any, Throwable, BitPosEvent] =
      ZIO(state.bitPos0b).map(BitPosEvent)
  }
  case class BitPosEvent(pos: Long) extends Event
}
