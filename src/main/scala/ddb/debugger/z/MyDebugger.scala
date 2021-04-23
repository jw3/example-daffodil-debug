package ddb.debugger.z

import ddb.debugger.api.{MultiEvent, Step}
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.processors.parsers.{PState, Parser}
import zio._

class MyDebugger(cs: CStream, es: EProducer) extends Debugger {
  private val rt = zio.Runtime.default

  override def init(state: PState, processor: Parser): Unit = println("[init]")
  override def fini(processor: Parser): Unit = println("[fini]")

  // todo;; there may need to be tie-in to the other hooks in the Debugger interface
  //        look in the interactive debugger to see where and when the others are used
  override def endElement(state: PState, processor: Parser): Unit =
    rt.unsafeRunSync(step(state, processor))

  /**
    * we control the debug process with a syncronous flow of commands that produce events
    */
  def step(state: PState, processor: Processor) =
    cs.take.flatMap {
      case s @ Step(_) =>

        s.run(state, processor)
          //.tap(e => putStrLn(s"===== Stepping ====="))
          .flatMap {
            case e: MultiEvent => es.publishAll(e.events()) *> es.publish(e)
            case e             => es.publish(e)
          }
      case _ => ZIO.unit
    }
}
