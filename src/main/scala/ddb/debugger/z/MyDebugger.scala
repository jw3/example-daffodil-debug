package ddb.debugger.z

import ddb.debugger.api.{MultiEvent, Step}
import ddb.debugger.z.cmdq.CmdQueue
import ddb.debugger.z.ebus.Eventbus
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.processors.parsers.{PState, Parser}
import zio._

class MyDebugger()(implicit rt: DebuggerRuntime) extends Debugger {
  override def init(state: PState, processor: Parser): Unit = println("[init]")
  override def fini(processor: Parser): Unit = println("[fini]")

  // todo;; there may need to be tie-in to the other hooks in the Debugger interface
  //        look in the interactive debugger to see where and when the others are used
  override def endElement(state: PState, processor: Parser): Unit = step(state, processor)

  /**
    * we control the debug process with a syncronous flow of commands that produce events
    */
  def step(state: PState, processor: Processor) =
    rt.unsafeRunSync(
      for {
        q <- CmdQueue.get()
        _ <- q.take.flatMap {
          case s @ Step(_) =>
            s.run(state, processor)
              //.tap(e => putStrLn(s"===== Stepping ====="))
              .flatMap {
                case e: MultiEvent => Eventbus.pubAll(e.events()) *> Eventbus.pub(e)
                case e             => Eventbus.pub(e)
              }
          case _ => ZIO.unit
        }
      } yield ()
    )
}
