package ddb.debugger.z

import ddb.debugger.api.{Command, Event, Step}
import org.apache.daffodil.sapi.Daffodil
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import zio._
import zio.duration.durationInt
import zio.stream._

import java.io.ByteArrayInputStream

/**
 * execute a debugging session
 * simulate receiving step commands from an input control
 * output debugging state info to simulated displays
 */
object main extends scala.App {
  val schema = getClass.getResource("/sch01.dfdl.xsd")
  val bytes = "012345".getBytes

  val c = Daffodil.compiler()
  val pf = c.compileSource(schema.toURI)
  pf.getDiagnostics.filter(_.isError).foreach(println)

  val app = for {
    cq <- Queue.unbounded[Command[_]]
    es <- Hub.unbounded[Event]

    // simulate an input control
    mc = MyControl(cq)
    _ <- mc.run(Stream.fromHub(es)).fork

    // simulate some output views
    _ <- MyInfoSetDisplay().run(Stream.fromHub(es)).fork
    _ <- MyBitPosDisplay().run(Stream.fromHub(es)).fork

    // simulate a view that maintains state
    prev <- Ref.make("")
    _ <- MyDiffingInfoSetDisplay(prev).run(Stream.fromHub(es)).fork

    // simulate steps every 2 seconds
    stepper = ZIO.sleep(2000.millis) *> mc.step()
    _ <- stepper.forever.fork

    // the debugger gets the command queue and event stream
    dp = pf.onPath("/").withDebugger(new MyDebugger(cq, es)).withDebugging(true)

    // the program will end when the parsing IO completes
    _ <- IO {
      dp.parse(
        new InputSourceDataInputStream(new ByteArrayInputStream(bytes)),
        new XMLTextInfosetOutputter(System.out, true)
      )
    }

  } yield ()

  Runtime.default.unsafeRun(app)
}
