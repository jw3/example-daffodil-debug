package ddb.debugger

import ddb.debugger.api.{Command, Event}
import zio.{Hub, Queue}
import zio.stream.ZStream

package object z {
  type EProducer = Queue[Event]
  type CProducer = Queue[Command[_]]
  type EStream = ZStream[Any, Nothing, Event]
  type CStream = ZStream[Any, Nothing, Command[_]]
}
