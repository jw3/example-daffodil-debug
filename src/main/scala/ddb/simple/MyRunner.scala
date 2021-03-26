package ddb.simple

import org.apache.daffodil.sapi.debugger.DebuggerRunner

class MyRunner( ) extends DebuggerRunner {
  def init(): Unit = println("[init]")

  val repeats = Stream.continually(Seq("step", "info infoset").toStream).flatten.iterator

  def getCommand(): String = {
    val cmd = repeats.next()
    println(s"[getCommand] $cmd")
    cmd
  }

  def lineOutput(line: String): Unit = println(s"[lineOutput] $line")

  def fini(): Unit = println("[fini]")
}
