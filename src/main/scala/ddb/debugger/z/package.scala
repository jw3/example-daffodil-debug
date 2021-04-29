package ddb.debugger

import ddb.debugger.api.{Command, Event}
import ddb.debugger.z.cmdq.CmdQueue
import ddb.debugger.z.compiler.CompilerAPI
import ddb.debugger.z.ebus.Eventbus
import ddb.debugger.z.history.History
import scalafx.application.Platform.runLater
import zio._
import zio.clock.Clock
import zio.console.Console
import zio.stream.ZStream

package object z {
  type EStream = ZStream[Any, Nothing, Event]
  type CStream = ZStream[Any, Nothing, Command[_]]

  type DebuggerRuntime =
    Runtime.Managed[
      Console with Clock with Has[CompilerAPI.Service] with Has[Eventbus.Service] with Has[CmdQueue.Service] with Has[History.Service]
    ]

  object FxIO {
    def apply[A](a: => Unit): Task[Unit] = IO {
      runLater(a)
    }
  }
}
