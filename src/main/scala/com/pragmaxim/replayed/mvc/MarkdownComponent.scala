package com.pragmaxim.replayed.mvc

import com.pragmaxim.replayed.event._
import eu.henkelmann.actuarius.Transformer
import monifu.concurrent.Scheduler
import monifu.reactive.channels.ObservableChannel
import monifu.reactive.{Observable, Ack}
import monifu.reactive.Ack.Continue
import org.scalajs.dom.ext.PimpedHtmlCollection
import org.scalajs.dom._
import org.scalajs.dom.html.{Div, IFrame}

import scala.concurrent.Future

class MarkdownComponent(val channel: ObservableChannel[RxEvent, RxEvent])(implicit s: Scheduler) extends Component {
  object MarkdownTransformer extends Transformer

  val me = document.getElementById("e_markdown").asInstanceOf[IFrame]
  val ee = document.getElementById("e_editor").asInstanceOf[Div]
  val lines = document.getElementById("e_lines").asInstanceOf[Div]

  def navbarHeight = document.getElementById("e_navbar").asInstanceOf[Div].clientHeight
  def bodyHeight = document.body.clientHeight
  def editorHeight = (bodyHeight - navbarHeight) + "px"

  ee.style.height = editorHeight
  me.style.height = editorHeight

  var change = false

  /**
   * This is a workaround for copy/pasting - browsers show empty line even without <br> but copy/paste requires <br> to consider empty lines
   * It is also a place to do some post/pre processing
   */
  private def addOrRemoveLineBreaks(textContent: String, l: Element): Unit = {
    val breaks = l.getElementsByTagName("br")
    if (textContent.isEmpty && breaks.length == 0)
      l.appendChild(document.createElement("br"))

    if (!textContent.isEmpty && breaks.length > 0)
      l.removeChild(breaks(0))
  }

  def onSubscribe(): Unit = {
    import scala.concurrent.duration._
    Observable.interval(150.millis).foreach { tick =>
      if (change) {
        change = false
        val text = lines.children.map { l =>
          val span = l.firstElementChild
          val textContent = span.textContent
          addOrRemoveLineBreaks(textContent, l)
          textContent
        }.toList

        me.innerHTML = MarkdownTransformer(text)
      }
      ee.style.height = editorHeight
      me.style.height = editorHeight
    }
  }

  val componentName = "markdown"

  def onComplete(): Unit = {}

  def onNextSafe(elem: RxEvent): Future[Ack] = elem match {
    case (_: SideEffect | DocLoaded | UndoEvent | RedoEvent) =>
      change = true
      Continue
    case _ =>
      Continue

  }
}

object MarkdownComponent {
  def apply(channel: ObservableChannel[RxEvent, RxEvent])(implicit s: Scheduler): MarkdownComponent = new MarkdownComponent(channel)
}