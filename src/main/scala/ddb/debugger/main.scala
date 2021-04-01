package ddb.debugger

import ddb.debugger.model.Step
import org.apache.daffodil.sapi.Daffodil
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import zio._

import java.io.ByteArrayInputStream

object main extends scala.App {
  val schema = getClass.getResource("/sch01.dfdl.xsd")
  val data = new ByteArrayInputStream("12345".getBytes)

  val c = Daffodil.compiler()
  val pf = c.compileSource(schema.toURI)
  pf.getDiagnostics.filter(_.isError).foreach(println)

  val app = for {
    eq <- Queue.unbounded[Event]
    cq <- Queue.unbounded[Command[_]]
    _ <- cq.offer(Step.default) // todo;; autostepping for demo

    eloop = for {
      msg <- eq.take
      _ <- cq.offer(Step.default) // todo;; autostepping for demo
    } yield {
      println(msg)
    }
    _ <- eloop.forever.fork

    debugger = new MyDebugger(cq, eq)
    dp = pf.onPath("/").withDebugging(true).withDebugger(debugger)
    _ <- IO {
      dp.parse(new InputSourceDataInputStream(data), new XMLTextInfosetOutputter(System.out, true))
    }
  } yield ()

  Runtime.default.unsafeRun(app)
}
