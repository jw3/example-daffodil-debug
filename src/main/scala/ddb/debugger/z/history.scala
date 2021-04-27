package ddb.debugger.z

import ddb.debugger.api.StepComplete
import ddb.debugger.z.ebus.Eventbus
import zio._
import zio.stream._

object history {
  type HistoryEvent = StepComplete
  type HistoryList = List[HistoryEvent]
  type HistoryStream = ZStream[Any, Nothing, (HistoryEvent, Long)]

  type History = Has[History.Service]
  object History {
    trait Service {
      def size(): UIO[Int]
      def all(): UIO[HistoryList]
      def sub(): HistoryStream
      def record(): URIO[Eventbus, Unit]
    }

    def make(): ZLayer[Eventbus, Throwable, Has[Service]] = {
      for {
        history <- Hub.unbounded[HistoryEvent]
        list <- Ref.make(List.empty[HistoryEvent])
      } yield new Service {
        def size(): UIO[Int] = history.size
        def all(): UIO[HistoryList] = list.get
        def sub(): HistoryStream = Stream.fromHub(history).zipWithIndex
        def record(): URIO[Eventbus, Unit] =
          Eventbus
            .sub()
            .flatMap(_.foreach {
              case e @ StepComplete(_) => list.update(_ :+ e) *> history.publish(e)
              case _                   => ZIO.unit
            })
      }
    }.toLayer

    def size(): URIO[History, Int] = ZIO.accessM(_.get.size())
    def all(): URIO[History, HistoryList] = ZIO.accessM(_.get.all())
    def sub(): URIO[History, HistoryStream] = ZIO.accessM(e => ZIO.succeed(e.get.sub()))
    def record(): URIO[History with Eventbus, Unit] = ZIO.accessM(_.get.record())
  }
}
