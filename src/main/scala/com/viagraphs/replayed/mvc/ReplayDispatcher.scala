package com.viagraphs.replayed.mvc

import com.viagraphs.replayed.event.RxEvent
import com.viagraphs.replayed.RichHTMLElement
import monifu.concurrent.Scheduler
import monifu.reactive.Ack.Continue
import monifu.reactive.BufferPolicy.Unbounded
import monifu.reactive.channels.SubjectChannel
import monifu.reactive._
import org.scalajs.dom.{document, console, Event}
import org.scalajs.dom.html.Span
import com.viagraphs.replayed.RichNode

import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.control.NonFatal

class ReplayDispatcher private (policy: BufferPolicy, s: Scheduler) extends SubjectChannel[RxEvent, RxEvent](PipeSubject[RxEvent]()(s), policy)(s)
object ReplayDispatcher {
  def apply(fn: (SubjectChannel[RxEvent, RxEvent]) => Iterable[Component], bufferPolicy: BufferPolicy = Unbounded)(implicit s: Scheduler): SubjectChannel[RxEvent, RxEvent] = {
    val dispatcher = new ReplayDispatcher(bufferPolicy, s)
    val components = fn(dispatcher)
    components.foreach { component =>
      dispatcher.subscribe(component)(s)
    }
    components.foreach(_.onSubscribe())
    dispatcher
  }
}

abstract class Component extends Observer[RxEvent] {
  def channel: SubjectChannel[RxEvent, RxEvent]
  def componentName: String
  def onSubscribe(): Unit
  def onNext(elem: RxEvent): Future[Ack] = {
    try {
      onNextSafe(elem)
    } catch {
      case NonFatal(ex) => 
        onError(ex)
        Continue
    }
  }
  def onNextSafe(elem: RxEvent): Future[Ack]
  def onComplete(): Unit
  def onError(ex: Throwable): Unit = {
    console.warn(s"Component $componentName error : $ex\n${ex.getCause}")
  }
}

sealed trait Switch 
case object off extends Switch
case object on extends Switch
case object toggle extends Switch


abstract class UiComponent extends Component {
  def countDown(from: Int, delay: FiniteDuration): Observable[Long] = {
    import monifu.reactive.internals.FutureAckExtensions
    Observable.create { subscriber =>
      implicit val s = subscriber.scheduler
      val o = subscriber.observer

      var counter = from
      s.scheduleRecursive(Duration.Zero, delay, { reschedule =>
        o.onNext(counter).onContinue {
          counter -= 1
          reschedule()
        }
      })
    }
  }

  def measure(input: String, className: String): Double = {
    val tmp = document.createElement("span").markup[Span] { lines =>
      lines.className = className
    }
    tmp.innerHTML = input.replace("/(<([^>]+)>)/ig", "")
    document.body.appendChild(tmp)
    val theWidth = tmp.getBoundingClientRect().width
    document.body.removeChild(tmp)
    theWidth
  }

  def dropdown(switch: Switch, targetClass: String, toggleClass: String) = (e:Event) => {
    import org.scalajs.dom.ext.PimpedHtmlCollection
    e.srcElement.forAncestorWithClass(targetClass) { listItem =>
      listItem.parentElement.children.filter(_ != listItem).foreach { siblingLi =>
        val siblingCl = siblingLi.classList
        if (siblingCl.contains(toggleClass))
          siblingCl.remove(toggleClass)
      }
      val cl = listItem.classList
      switch match {
        case `toggle` =>
          if (cl.contains(toggleClass)) cl.remove(toggleClass) else cl.add(toggleClass)
        case `on` =>
          if (!cl.contains(toggleClass)) cl.add(toggleClass)
        case `off` =>
          if (cl.contains(toggleClass)) cl.remove(toggleClass)
      }
    }
  }

}