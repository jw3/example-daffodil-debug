package ddb.debugger.z

import ddb.debugger.api.Command
import zio.{Has, Queue, UIO, ZIO, ZLayer, stream}

object cmdq {
  type CmdQueue = Has[CmdQueue.Service]
  object CmdQueue {
    trait Service {
      def get(): UIO[Queue[Command[_]]]
      def sub(): CStream
    }

    def make(): ZLayer[Any, Throwable, Has[Service]] = {
      for {
        queue <- Queue.unbounded[Command[_]]
      } yield new Service {
        def get(): UIO[Queue[Command[_]]] = UIO.succeed(queue)
        def sub(): CStream = stream.Stream.fromQueue(queue)
      }
    }.toLayer

    def get(): ZIO[CmdQueue, Nothing, Queue[Command[_]]] = ZIO.accessM(_.get.get())
    def sub(): ZIO[CmdQueue, Nothing, CStream] = ZIO.accessM(e => ZIO.succeed(e.get.sub()))
  }
}
