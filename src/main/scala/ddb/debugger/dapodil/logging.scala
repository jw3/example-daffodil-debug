package ddb.debugger.dapodil

import cats.Show
import com.microsoft.java.debug.core.protocol.Messages._
import com.microsoft.java.debug.core.protocol.Events
import com.microsoft.java.debug.core.protocol.Events._
import com.microsoft.java.debug.core.protocol.Responses.InitializeResponseBody
import com.microsoft.java.debug.core.protocol.JsonUtils

object logging {
  implicit val requestShow: Show[Request] =
    request => s"#${request.seq} ${request.command} ${request.arguments}"

  implicit val responseShow: Show[Response] =
    response =>
      response.body match {
        case body: InitializeResponseBody => s"#${response.request_seq} ${response.command} ${JsonUtils.toJson(body)}"
        case _                            => s"#${response.request_seq} ${response.command}"
      }

  implicit val eventShow: Show[DebugEvent] = {
    case event: Events.StoppedEvent => s"${event.`type`} ${event.reason}"
    case event: Events.ThreadEvent  => s"${event.`type`} ${event.reason}"
    case event                      => s"${event.`type`}"
  }
}
