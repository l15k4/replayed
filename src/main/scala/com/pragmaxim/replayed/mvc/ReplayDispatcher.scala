package com.pragmaxim.replayed.mvc

import java.util.concurrent.TimeUnit

import com.pragmaxim.replayed.event.RxEvent
import com.pragmaxim.replayed.{RichHTMLElement, RichNode}
import monifu.concurrent.Scheduler
import monifu.reactive.Ack.{Cancel, Continue}
import monifu.reactive.OverflowStrategy.Unbounded
import monifu.reactive._
import monifu.reactive.channels.ObservableChannel
import monifu.reactive.observers.BufferedSubscriber
import org.scalajs.dom.html.Span
import org.scalajs.dom.{Event, console, document}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

class ReplayDispatcher private (overflowStrategy: OverflowStrategy.Synchronous, s: Scheduler) extends ObservableChannel[RxEvent, RxEvent] {

  val subject = PipeSubject[RxEvent]()(s)

  private[this] val channel = BufferedSubscriber(
    Subscriber(subject, s), overflowStrategy)

  final def onSubscribe(subscriber: Subscriber[RxEvent]): Unit = {
    subject.onSubscribe(subscriber)
  }

  final def pushNext(elems: RxEvent*): Unit = {
    for (elem <- elems) channel.onNext(elem)
  }

  final def pushComplete(): Unit = {
    channel.onComplete()
  }

  final def pushError(ex: Throwable): Unit = {
    channel.onError(ex)
  }

}
object ReplayDispatcher {
  def apply(fn: (ObservableChannel[RxEvent, RxEvent]) => Iterable[Component], bufferPolicy: OverflowStrategy.Synchronous = Unbounded)(implicit s: Scheduler): ObservableChannel[RxEvent, RxEvent] = {
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
  def channel: ObservableChannel[RxEvent, RxEvent]
  def componentName: String
  def onSubscribe(): Unit
  def onNext(elem: RxEvent): Future[Ack] = {
    try {
      onNextSafe(elem)
    } catch {
      case NonFatal(ex) => 
        onError(ex)
        Cancel
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
  def countDown(from: Long, ms: Long): Observable[Long] = {
    Observable.create { o =>
      import o.{scheduler => s}

      s.scheduleOnce(0L.seconds) {new Runnable { self =>
        private[this] var counter = from

        def scheduleNext(r: Try[Ack]): Unit = r match {
          case Continue.IsSuccess =>
            counter -= 1
            s.scheduleOnce(0L, unit = TimeUnit.MILLISECONDS, r = self)

          case Failure(ex) =>
            s.reportFailure(ex)
          case _ =>
            () // do nothing
        }

        def run(): Unit = {
          val ack = o.onNext(counter)

          if (ack.isCompleted)
            scheduleNext(ack.value.get)
          else
            ack.onComplete(scheduleNext)
        }
      }}
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