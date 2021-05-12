package ddb.debugger.dapodil

import cats.Show
import cats.effect._
import cats.syntax.all._
import com.microsoft.java.debug.core.protocol.Messages._
import com.microsoft.java.debug.core.protocol.Events.DebugEvent

object logging {
  implicit val requestShow: Show[Request] =
    request => s"#${request.seq} ${request.command} ${request.arguments}"

  implicit val responseShow: Show[Response] =
    response => s"#${response.request_seq} ${response.command}"

  implicit val eventShow: Show[DebugEvent] =
    event => s"${event.`type`}"

  implicit class DebugOps[A](fa: IO[A]) {
    def debug(): IO[A] =
      fa.flatTap(a => IO.println(s"[${Thread.currentThread.getName}] $a"))
  }
}
