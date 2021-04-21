package ddb.debugger.z

import ddb.debugger.api.{Command, Event}
import ddb.debugger.z.compiler.CompilerAPI
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import zio._
import zio.clock.Clock
import zio.console.Console
import zio.stream._

import java.io.ByteArrayInputStream

/**
  * execute a debugging session
  * simulate receiving step commands from an input control
  * output debugging state info to simulated displays
  */
object main extends scala.App {
  val schema = getClass.getResource("/com/mitre/jpeg/xsd/jpeg.dfdl.xsd")
  val bytes = getClass.getResourceAsStream("/works.jpg").readAllBytes()

  // set up the environment with the Compiler API
  val deps = Console.live >+> Clock.live >+> CompilerAPI.make()

  val app = for {
    cq <- Queue.unbounded[Command[_]]
    es <- Hub.unbounded[Event]

    // simulate an input control
    mc = MyControl(cq)
    _ <- mc.run(Stream.fromHub(es)).fork

    // simulate some output views
    infosetView = MyInfoSetDisplay()
    _ <- infosetView.run(Stream.fromHub(es)).fork
    bitposView = MyBitPosDisplay(bytes)
    _ <- bitposView.run(Stream.fromHub(es)).fork
    pathView = MyPathDisplay()
    _ <- pathView.run(Stream.fromHub(es)).fork
    varsView = MyVariablesDisplay()
    _ <- varsView.run(Stream.fromHub(es)).fork

    // simulate a view that maintains state
    prev <- Ref.make("")
    differ = MyDiffingInfoSetDisplay(prev)
    _ <- differ.run(Stream.fromHub(es)).fork

    // fork off the gui with all the views
    _ <- gui.run(cq, infosetView, bitposView, differ, pathView, varsView).fork

    // the debugger gets the command queue and event stream
    dp <- CompilerAPI.compile(schema.toURI).map { p =>
      p.withDebugger(new MyDebugger(cq, es)).withDebugging(true)
    }

    // the program will end when the parsing IO completes
    _ <- IO {
      dp.parse(
        new InputSourceDataInputStream(new ByteArrayInputStream(bytes)),
        new XMLTextInfosetOutputter(System.out, true)
      )
    }

  } yield ()

  Runtime.unsafeFromLayer(deps).unsafeRun(app)
}
