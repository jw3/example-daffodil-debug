package ddb.actor

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import ddb.actor.MyRunner.StepOutput

object main extends App {
  val system = ActorSystem()

  val info = system.actorOf(Props[StepDisplay])
  val vars = system.actorOf(Props[StepDisplay])
  val data = system.actorOf(Props[StepDisplay])
  val runner = system.actorOf(MyRunner.props(info, vars, data))
}

class StepDisplay extends Actor with ActorLogging {
  def receive: Receive = {
    case StepOutput(txt) => log.info(txt)
  }
}
