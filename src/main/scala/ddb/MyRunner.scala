package ddb

import org.apache.daffodil.sapi.debugger.DebuggerRunner

class MyRunner( ) extends DebuggerRunner {
  def init(): Unit = println("[init]")

  def getCommand(): String = {
    println("[getCommand]")
    "step"
  }

  def lineOutput(line: String): Unit = println(s"[lineOutput] $line")

  def fini(): Unit = println("[fini]")
}
