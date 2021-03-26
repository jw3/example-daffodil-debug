package ddb.zio

import ddb.zio.MyRunner.repeats
import org.apache.daffodil.sapi.debugger.DebuggerRunner
import zio.Queue

object MyRunner {
  val repeats = Stream.continually(Seq("step", "info infoset").toStream).flatten.iterator
}

class MyRunner(queue: Queue[String]) extends DebuggerRunner {
  private val rt = zio.Runtime.default

  def getCommand(): String = {
    val output = buffer.mkString
    buffer.clear()
    rt.unsafeRun(queue.offer(output))
    repeats.next()
  }

  val buffer = new StringBuilder
  def lineOutput(line: String): Unit =
    buffer.append(s"$line\n")

  def init(): Unit = println("[init]")

  def fini(): Unit = println("[fini]")
}
