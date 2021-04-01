package ddb.actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import ddb.actor.MyRunner.{GetCommand, LineOutput, StepOutput}
import org.apache.daffodil.sapi.Daffodil
import org.apache.daffodil.sapi.debugger.DebuggerRunner
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream

import java.io.ByteArrayInputStream
import java.util.concurrent.PriorityBlockingQueue
import scala.concurrent.Future

object MyRunner {
  def props(infoset: ActorRef, variables: ActorRef, data: ActorRef) = Props(new MyRunner(infoset, variables, data))

  case object GetCommand
  case class LineOutput(txt: String)
  case class StepOutput(txt: String)

  private val repeats = Stream.continually(Seq("step", "info infoset").toStream).flatten.iterator
}

class MyRunner(infoset: ActorRef, variables: ActorRef, data: ActorRef) extends Actor with Stash with ActorLogging {
  val cmdq = new PriorityBlockingQueue[String]

  val schema = getClass.getResource("/variables_01.dfdl.xsd")
  val input = new ByteArrayInputStream("0,1,2".getBytes)

  val c = Daffodil.compiler()
  val pf = c.compileSource(schema.toURI)
  pf.getDiagnostics.filter(_.isError).foreach(println)

  val dp = pf.onPath("/").withDebugging(true).withDebuggerRunner(new MyDebuggerRunner)

  Future {
    dp.parse(new InputSourceDataInputStream(input), new XMLTextInfosetOutputter(System.out, true))
  }(context.dispatcher)

  def stepping: Receive = {
    unstashAll()

    {
      case GetCommand =>
        cmdq.add("step")
        context.become(callInfoset)
    }
  }

  def callInfoset: Receive = {
    case GetCommand =>
      cmdq.add("info infoset")
      context.become(handleInfoset)
  }

  def handleInfoset: Receive = {
    val buffer = new StringBuilder

    {
      case LineOutput(txt) =>
        buffer.append(s"$txt\n")
      case GetCommand =>
        stash()
        infoset ! StepOutput(buffer.mkString)
        context.become(callVariables)
    }
  }

  def callVariables: Receive = {
    unstashAll()

    {
      case GetCommand =>
        cmdq.add("info variables")
        context.become(handleVariables)
    }
  }

  def handleVariables: Receive = {
    val buffer = new StringBuilder

    {
      case LineOutput(txt) =>
        buffer.append(s"$txt\n")
      case GetCommand =>
        stash()
        variables ! StepOutput(buffer.mkString)
        context.become(callData)
    }
  }

  def callData: Receive = {
    unstashAll()

    {
      case GetCommand =>
        cmdq.add("info data")
        context.become(handleData)
    }
  }

  def handleData: Receive = {
    val buffer = new StringBuilder

    {
      case LineOutput(txt) =>
        buffer.append(s"$txt\n")
      case GetCommand =>
        stash()
        data ! StepOutput(buffer.mkString)
        context.become(stepping)
    }
  }

  def receive: Receive = stepping

  ///

  class MyDebuggerRunner extends DebuggerRunner {
    def getCommand(): String = {
      self ! GetCommand
      cmdq.take()
    }

    def lineOutput(line: String): Unit =
      self ! LineOutput(line)

    def init(): Unit = println("[init]")

    def fini(): Unit = println("[fini]")
  }
}
