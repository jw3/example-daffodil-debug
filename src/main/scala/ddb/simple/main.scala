package ddb.simple

import org.apache.daffodil.sapi.Daffodil
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream

import java.io.ByteArrayInputStream

object main extends App {
  val schema = getClass.getResource("/sch01.dfdl.xsd")
  val data = new ByteArrayInputStream("12345".getBytes)

  val c = Daffodil.compiler()
  val pf = c.compileSource(schema.toURI)
  pf.getDiagnostics.filter(_.isError).foreach(println)

  val runner = new MyRunner()
  val dp = pf.onPath("/").withDebugging(true).withDebuggerRunner(runner)


  val pr = dp.parse(new InputSourceDataInputStream(data), new XMLTextInfosetOutputter(System.out, true))
  pr.getDiagnostics.foreach(println)
}
