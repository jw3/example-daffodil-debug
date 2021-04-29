package ddb.debugger.dapodil

import ddb.debugger.z.compiler
import java.net.URI
import cats.effect.IO
import cats.syntax.all._
import org.apache.daffodil.sapi._

trait Compiler {
  def compile(schema: URI): IO[DataProcessor]
}

object Compiler {
  def apply: Compiler =
    new Compiler {
      def compile(schema: URI): IO[DataProcessor] =
        IO.blocking(
            Daffodil
              .compiler()
              .compileSource(schema)
          )
          .ensureOr(pf => compiler.CompilationFailed(pf.getDiagnostics))(!_.isError)
          .map(_.onPath("/"))
    }
}
