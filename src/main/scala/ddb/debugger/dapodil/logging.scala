package ddb.debugger.dapodil

import cats.Show
import com.microsoft.java.debug.core.protocol.Messages._
import com.microsoft.java.debug.core.protocol.Events.DebugEvent

object logging {
  implicit val requestShow: Show[Request] =
    request => s"#${request.seq} ${request.command} ${request.arguments}"

  implicit val responseShow: Show[Response] =
    response => s"#${response.request_seq} ${response.command} ${response.body}"

  implicit val eventShow: Show[DebugEvent] =
    event => s"${event}"
}
