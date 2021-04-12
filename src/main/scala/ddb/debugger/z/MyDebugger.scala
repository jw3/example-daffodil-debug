package ddb.debugger.z

import ddb.debugger.api.{MultiEvent, Step}
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.processors.parsers.{PState, Parser}
import zio._
import zio.console.putStrLn

class MyDebugger(cs: CStream, es: EProducer) extends Debugger {
  private val rt = zio.Runtime.default

  override def init(state: PState, processor: Parser): Unit = println("[init]")
  override def fini(processor: Parser): Unit = println("[fini]")

  override def endElement(state: PState, processor: Parser): Unit = step(state)

  /**
    * we control the debug process with a syncronous flow of commands that produce events
    */
  def step(state: PState) =
    rt.unsafeRunSync(cs.take.flatMap {
      case s @ Step(_) =>
        s.run(state)
          .tap(e => putStrLn(s"===== Stepping ====="))
          .flatMap {
            case e: MultiEvent => es.publishAll(e.events()) *> es.publish(e)
            case e             => es.publish(e)
          }
      case _ =>
        println("NOT RUNNING")
        ZIO.succeed()
    })
}
