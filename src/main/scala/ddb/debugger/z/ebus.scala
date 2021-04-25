package ddb.debugger.z

import ddb.debugger.api.Event
import zio.{Has, Hub, UIO, URIO, ZIO, ZLayer, stream}

object ebus {
  type Eventbus = Has[Eventbus.Service]
  object Eventbus {
    trait Service {
      def pub(e: Event): UIO[Boolean]
      def pubAll(e: Iterable[Event]): UIO[Boolean]
      def sub(): EStream
    }

    def make(): ZLayer[Any, Throwable, Has[Service]] = {
      for {
        hub <- Hub.unbounded[Event]
      } yield new Service {
        def pub(e: Event): UIO[Boolean] = hub.publish(e)
        def pubAll(e: Iterable[Event]): UIO[Boolean] = hub.publishAll(e)
        def sub(): EStream = stream.Stream.fromHub(hub)
      }
    }.toLayer

    def pub(e: Event): URIO[Eventbus, Boolean] = ZIO.accessM(_.get.pub(e))
    def pubAll(e: Iterable[Event]): URIO[Eventbus, Boolean] = ZIO.accessM(_.get.pubAll(e))
    def sub(): URIO[Eventbus, EStream] = ZIO.accessM(e => ZIO.succeed(e.get.sub()))
  }
}
