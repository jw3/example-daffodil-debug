package ddb.debugger.cmd

import ddb.debugger.Command
import ddb.debugger.event.{InfosetEvent, NoInfoset, ViewInfosetEvent}
import org.apache.daffodil.infoset.{DIDocument, DIElement, InfosetElement, InfosetWalker, XMLTextInfosetOutputter}
import org.apache.daffodil.processors.parsers.PState
import zio.ZIO

object infoset {
  case object ShowInfoset extends Command[InfosetEvent] {
    def run(state: PState): ZIO[Any, Throwable, InfosetEvent] =
      ZIO(state.infoset).map {
        case d: DIDocument if d.contents.isEmpty => NoInfoset
        case node                                => ViewInfosetEvent(infosetToString(node))
      }

    private def infosetToString(ie: InfosetElement): String = {
      val bos = new java.io.ByteArrayOutputStream()
      val xml = new XMLTextInfosetOutputter(bos, true)
      val iw = InfosetWalker(
        ie.asInstanceOf[DIElement],
        xml,
        walkHidden = false,
        ignoreBlocks = true,
        releaseUnneededInfoset = false
      )
      iw.walk(lastWalk = true)
      bos.toString("UTF-8")
    }
  }
}
