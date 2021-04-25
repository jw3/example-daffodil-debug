package ddb.debugger.z

import ddb.debugger.api.MultiEvent
import ddb.debugger.z.cmdq.CmdQueue
import ddb.debugger.z.compiler.CompilerAPI
import ddb.debugger.z.ebus.Eventbus
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import zio._
import zio.clock.Clock
import zio.console.Console

import java.io.ByteArrayInputStream

/**
  * execute a debugging session
  * simulate receiving step commands from an input control
  * output debugging state info to simulated displays
  */
object main extends scala.App {
  val schema = getClass.getResource("/jpeg.dfdl.xsd")
  val bytes = getClass.getResourceAsStream("/works.jpg").readAllBytes()

  // set up the debugger runtime environment
  implicit val rt: DebuggerRuntime = Runtime.unsafeFromLayer(
    Console.live >+> Clock.live >+> CompilerAPI.make() >+> Eventbus.make() >+> CmdQueue.make()
  )

  val app = for {
    // a view that maintains previous state of infoset
    prev <- Ref.make("")
    differ = MyDiffingInfoSetDisplay(prev)
    _ <- differ.run().fork

    // various output views
    infosetView = MyInfoSetDisplay()
    _ <- infosetView.run().fork
    bitposView = MyBitPosDisplay(bytes)
    _ <- bitposView.run().fork
    pathView = MyPathDisplay()
    _ <- pathView.run().fork
    varsView = MyVariablesDisplay()
    _ <- varsView.run().fork

    // an input control that maintains state
    history <- Ref.make(List.empty[MultiEvent])
    slider = MySliderControl(history)
    _ <- slider.run().fork

    stepCount = MyStepCountDisplay(history)
    _ <- stepCount.run().fork

    // fork off the gui with all the views
    _ <- gui.run(infosetView, bitposView, differ, pathView, varsView, slider, stepCount).fork

    // compile the schema and install the debugger
    // the debugger will get all dependencies via environment
    dp <- CompilerAPI.compile(schema.toURI).map { p =>
      p.withDebugger(new MyDebugger()).withDebugging(true)
    }

    // the program will end when the parsing IO completes
    _ <- IO {
      dp.parse(
        new InputSourceDataInputStream(new ByteArrayInputStream(bytes)),
        new XMLTextInfosetOutputter(System.out, true)
      )
    }

  } yield ()

  rt.unsafeRun(app)
}
