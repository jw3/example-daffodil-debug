package ddb

import org.apache.daffodil.processors.parsers.PState
import zio.ZIO

package object debugger {
  // a command operating on the state produces an event
  // Command(state) => Event
  trait Command[E <: Event] {
    def run(state: PState): ZIO[Any, Throwable, E]
  }
  trait Event
  trait MultiEvent extends Event {
    def events(): Seq[Event]
  }
}
