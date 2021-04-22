package ddb.debugger.z

import org.apache.daffodil.sapi.{Daffodil, DataProcessor, Diagnostic}
import zio.{Has, IO, UIO, ZIO, ZLayer}

import java.net.URI

/**
  * Daffodil Compiler API service
  */
object compiler {
  case class CompilationFailed(seq: Seq[Diagnostic]) extends Exception

  type CompilerAPI = Has[CompilerAPI.Service]
  object CompilerAPI {
    trait Service {
      def compile(schema: URI): IO[CompilationFailed, DataProcessor]
    }

    def make(): ZLayer[Any, Throwable, Has[Service]] =
      ZIO(Daffodil.compiler()).map { compiler =>
        new Service {
          def compile(schema: URI): IO[CompilationFailed, DataProcessor] =
            UIO(compiler.compileSource(schema)).flatMap {
              case pf if pf.isError() => IO.fail(CompilationFailed(pf.getDiagnostics))
              case pf                 => IO.succeed(pf.onPath("/"))
            }
        }
      }.toLayer

    def compile(schema: URI): ZIO[CompilerAPI, CompilationFailed, DataProcessor] = ZIO.accessM(_.get.compile(schema))
  }
}
