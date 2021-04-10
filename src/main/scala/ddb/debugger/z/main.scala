package ddb.debugger.z

import ddb.debugger.api.{BitPosEvent, Command, Event, Step}
import org.apache.daffodil.sapi.Daffodil
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import zio.{IO, Queue, Runtime}

import java.io.ByteArrayInputStream

object main extends scala.App {
  val schema = getClass.getResource("/sch01.dfdl.xsd")
  val bytes = "012345".getBytes

  val c = Daffodil.compiler()
  val pf = c.compileSource(schema.toURI)
  pf.getDiagnostics.filter(_.isError).foreach(println)

  val app = for {
    eq <- Queue.unbounded[Event]
    cq <- Queue.unbounded[Command[_]]
    _ <- cq.offer(Step.default) // todo;; autostepping for demo

    eloop = for {
      e <- eq.take
      _ <- cq.offer(Step.default) // todo;; autostepping for demo
    } yield e match {
      case BitPosEvent(pos) =>
        val c = pos.toInt / 8 - 1
        val ch = bytes(c).toChar
        println(s"$e => value: $ch")
      case _ => println(e)
    }
    _ <- eloop.forever.fork

    debugger = new MyDebugger(cq, eq)
    dp = pf.onPath("/").withDebugger(debugger).withDebugging(true)
    _ <- IO {
      dp.parse(
        new InputSourceDataInputStream(new ByteArrayInputStream(bytes)),
        new XMLTextInfosetOutputter(System.out, true)
      )
    }
  } yield ()

  Runtime.default.unsafeRun(app)
}
