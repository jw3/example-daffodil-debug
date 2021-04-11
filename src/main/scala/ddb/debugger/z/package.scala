package ddb.debugger

import ddb.debugger.api.{Command, Event}
import zio.stream.ZStream
import zio.{Hub, Queue}

package object z {
  type EProducer = Hub[Event]
  type CProducer = Queue[Command[_]]
  type EStream = ZStream[Any, Nothing, Event]
  type CStream = Queue[Command[_]]
}
