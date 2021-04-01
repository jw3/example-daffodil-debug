package ddb.zio

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
    q <- Queue.unbounded[String]
    runner = new MyRunner(q)

    dp = pf.onPath("/").withDebugging(true).withDebuggerRunner(runner)
    pr = dp.parse(new InputSourceDataInputStream(data), new XMLTextInfosetOutputter(System.out, true))

    loop = for {
      msg <- q.take
    } yield {
      println(msg)
    }
    _ <- loop.forever
  } yield ()

  Runtime.default.unsafeRun(app)
}
