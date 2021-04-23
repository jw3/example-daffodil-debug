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
import scalafx.application.Platform

/**
  * execute a debugging session
  * simulate receiving step commands from an input control
  * output debugging state info to simulated displays
  */
object main extends zio.App {
  def run(args: List[String]) =
    app.provideLayer(deps).exitCode

  // set up the environment with the Compiler API
  val deps = Console.live >+> Clock.live >+> CompilerAPI.make()
  
  val app =
    for {
      cq <- Queue.unbounded[Command[_]]
      es <- Hub.unbounded[Event]

      _ = Platform.startup(() => ())

      // simulate an input control
      mc = MyControl(cq)
      _ <- mc.run(Stream.fromHub(es)).fork

      // simulate some output views
      infosetView = MyInfoSetDisplay()
      bytes = getClass.getResourceAsStream("/works.jpg").readAllBytes()
      bitposView = MyBitPosDisplay(bytes)
      pathView = MyPathDisplay()
      varsView = MyVariablesDisplay()

      // simulate a view that maintains state
      prev <- Ref.make("")
      differ = MyDiffingInfoSetDisplay(prev)
      
      // fork off the gui with all the views
      _ <- gui.run(cq, infosetView.control, bitposView.control, differ.control, pathView.control, varsView.control).fork
      
      // connect views to events
      _ <- infosetView.run(Stream.fromHub(es)).fork
      _ <- bitposView.run(Stream.fromHub(es)).fork
      _ <- pathView.run(Stream.fromHub(es)).fork
      _ <- varsView.run(Stream.fromHub(es)).fork
      _ <- differ.run(Stream.fromHub(es)).fork
      
      // the debugger gets the command queue and event stream
      schema = getClass.getResource("/com/mitre/jpeg/xsd/jpeg.dfdl.xsd")

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

}
