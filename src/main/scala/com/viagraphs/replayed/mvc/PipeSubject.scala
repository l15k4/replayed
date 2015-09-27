package com.viagraphs.replayed.mvc

import monifu.concurrent.Scheduler
import monifu.reactive.Ack.{Cancel, Continue}
import monifu.reactive.internals.{FutureAckExtensions, PromiseCounter}
import monifu.reactive.{Subscriber, Ack, Observer, Subject}
import scala.concurrent.Future

final class PipeSubject[T] private (implicit s: Scheduler) extends Subject[T,T] {
  private[this] val lock = new AnyRef
  private[this] var isCompleted = false
  private[this] var errorThrown: Throwable = null
  @volatile private[this] var subscriptions = Array.empty[Observer[T]]

  def subscribeFn(observer: Subscriber[T]): Unit = {
    lock.synchronized {
      if (!isCompleted)
        subscriptions = createSubscription(subscriptions, observer)
      else if (errorThrown ne null)
        observer.onError(errorThrown)
      else
        observer.onComplete()
    }
  }

  def onNext(elem: T): Future[Ack] = {
    if (!isCompleted) {
      val observers = subscriptions
      if (observers.nonEmpty)
        pipeTroughMany(observers, elem)
      else
        Continue
    }
    else
      Cancel
  }

  def onError(ex: Throwable) =
    lock.synchronized {
      if (!isCompleted) {
        isCompleted = true
        errorThrown = ex

        var idx = 0
        while (idx < subscriptions.length) {
          subscriptions(idx).onError(ex)
          idx += 1
        }
      }
    }

  def onComplete() =
    lock.synchronized {
      if (!isCompleted) {
        isCompleted = true

        var idx = 0
        while (idx < subscriptions.length) {
          subscriptions(idx).onComplete()
          idx += 1
        }
      }
    }

  private[this] def pipeTroughMany(array: Array[Observer[T]], elem: T): Future[Continue] = {
    val length = array.length
    def >>>(idx: Int = 0): Future[Continue] = {
      val obs = array(idx)
      obs.onNext(elem).flatMap {
         case Continue =>
           if (idx+1 < length)
              >>>(idx+1)
           else
             Continue
         case _ =>
           removeSubscription(obs)
           Continue
      }
    }
    >>>()
  }

  private[this] def removeSubscription(observer: Observer[T]): Unit =
    lock.synchronized {
      subscriptions = subscriptions.filter(_ != observer)
    }

  private[this] def createSubscription(observers: Array[Observer[T]], instance: Observer[T]): Array[Observer[T]] =
    lock.synchronized {
      if (!observers.contains(instance))
        observers :+ instance
      else
        observers
    }
}

object PipeSubject {
  def apply[T]()(implicit s: Scheduler): PipeSubject[T] = new PipeSubject[T]()
}
