package ddb

import org.apache.daffodil.sapi.Daffodil
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream

object main extends App {
  val schema = getClass.getResource("/com/mitre/jpeg/xsd/jpeg.dfdl.xsd")
  val data = getClass.getResource("/works.jpg")

  val c = Daffodil.compiler()
  val pf = c.compileSource(schema.toURI)
  pf.getDiagnostics.filter(_.isError).foreach(println)

  val runner = new MyRunner()
  val debugger = new MyDebugger(runner)
  val dp = pf.onPath("/").withDebugging(true).withDebugger(runner)

  val pr = dp.parse(new InputSourceDataInputStream(data.openStream()), new XMLTextInfosetOutputter(System.out, true))
  pr.getDiagnostics.foreach(println)
}
