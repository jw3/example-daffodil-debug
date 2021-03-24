package ddb

import org.apache.daffodil.events.EventHandler
import org.apache.daffodil.processors.parsers.{PState, Parser}

class MyDebugger(runner: MyRunner) extends EventHandler{
  override def init(state: PState, processor: Parser): Unit = runner.init()

  override def startElement(state: PState, processor: Parser): Unit = step()

  override def endElement(state: PState, processor: Parser): Unit = step()

  override def fini(processor: Parser): Unit = runner.fini

  def step() = {
    println("stepping")
  }
}
