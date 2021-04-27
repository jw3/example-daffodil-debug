package ddb.debugger.z

import ddb.debugger.z.cmdq.CmdQueue
import ddb.debugger.z.compiler.CompilerAPI
import ddb.debugger.z.ebus.Eventbus
import ddb.debugger.z.history.History
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import zio._
import zio.clock.Clock
import zio.console.Console

import java.io.ByteArrayInputStream

object main extends scala.App {
  val schema = getClass.getResource("/jpeg.dfdl.xsd")
  val bytes = getClass.getResourceAsStream("/works.jpg").readAllBytes()

  // set up the debugger runtime environment
  implicit val rt: DebuggerRuntime = Runtime.unsafeFromLayer(
    Console.live >+> Clock.live >+> CompilerAPI.make() >+> Eventbus.make() >+> CmdQueue.make() >+> History.make()
  )

  val app = for {
    // boot up the history recording stream
    // todo;; start when creating the layer?
    _ <- History.record().fork

    // various output views
    infosetView = MyInfoSetDisplay()
    _ <- infosetView.run().fork
    bitposView = MyBitPosDisplay(bytes)
    _ <- bitposView.run().fork
    pathView = MyPathDisplay()
    _ <- pathView.run().fork
    varsView = MyVariablesDisplay()
    _ <- varsView.run().fork
    stepCount = MyStepCountDisplay()
    _ <- stepCount.run().fork
    slider = MySliderControl()
    _ <- slider.run().fork

    // a view that naively maintains the previous infoset state
    prev <- Ref.make("")
    differ = MyDiffingInfoSetDisplay(prev)
    _ <- differ.run().fork

    // fork off the gui with all the views
    _ <- gui.run(infosetView, bitposView, differ, pathView, varsView, slider, stepCount).fork

    // compile the schema and install the debugger
    // the debugger will get all dependencies via environment
    dp <- CompilerAPI.compile(schema.toURI).map { p =>
      p.withDebugger(new MyDebugger()).withDebugging(true)
    }

    // todo;; the program ends when the parsing IO completes
    //        this breaks history replay after the last step
    _ <- IO {
      dp.parse(
        new InputSourceDataInputStream(new ByteArrayInputStream(bytes)),
        new XMLTextInfosetOutputter(System.out, true)
      )
    }

  } yield ()

  rt.unsafeRun(app)
}
