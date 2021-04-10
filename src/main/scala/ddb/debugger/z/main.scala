package ddb.debugger.z

import ddb.debugger.api.{Command, Event, Step}
import org.apache.daffodil.sapi.Daffodil
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import zio._
import zio.console._
import zio.duration.durationInt
import zio.stream._

import java.io.ByteArrayInputStream

object main extends scala.App {
  val schema = getClass.getResource("/sch01.dfdl.xsd")
  val bytes = "012345".getBytes

  val c = Daffodil.compiler()
  val pf = c.compileSource(schema.toURI)
  pf.getDiagnostics.filter(_.isError).foreach(println)

  val app = for {
    cq <- Queue.unbounded[Command[_]]
    es <- Queue.unbounded[Event]

    mc = MyControl(cq)
    _ <- mc.step()
    _ <- mc.run(Stream.fromQueue(es)).fork

    //      _ <- mc.run(Stream.fromHub(es)).forever.fork
//    _ <- MyInfoSetDisplay().run(Stream.fromQueue(es)).forever.fork
//    _ <- MyBitPosDisplay().run(Stream.fromQueue(es)).forever.fork

    debugger = new MyDebugger(Stream.fromQueue(cq), es)
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
