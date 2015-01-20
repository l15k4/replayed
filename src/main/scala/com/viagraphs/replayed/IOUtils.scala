package com.viagraphs.replayed

import org.scalajs.dom._

import scala.concurrent.{Promise, Future}
import upickle._

import scala.util.Try

object IOUtils {

  def readRemoteResource(url: String): Future[String] = {
    val reqPromise = Promise[String]()
    val request = new XMLHttpRequest()
    request.onload = (e: Event) => {
      reqPromise.trySuccess(request.responseText)
    }
    request.onerror = (e: Event) => {
      reqPromise.tryFailure(
        new Exception(s"Unable to load remote resource $url due to ${request.status}: ${request.statusText}")
      )
    }
    request.onabort = (e: Any) => {
      reqPromise.tryFailure(
        new Exception(s"Loading remote file was aborted")
      )
    }
    request.open("GET", url, true)
    request.send()
    reqPromise.future
  }

  def readFile(file: File): Future[String] = {
    val reqPromise = Promise[String]()
    val reader = new FileReader()
    reader.onload = (e: UIEvent) => {
      reqPromise.trySuccess(reader.result.asInstanceOf[String])
    }
    reader.onerror = (e: Event) => {
      reqPromise.tryFailure(
        new Exception(s"Unable to load file due to ${reader.error.name}")
      )
    }
    reader.onabort = (e: Event) => {
      reqPromise.tryFailure(
        new Exception("Reading file was aborted")
      )
    }
    reader.readAsText(file)
    reqPromise.future
  }

  def unmarshal(jsonOrText: String): Seq[Change] = {
    import com.viagraphs.idb.IdbSupport._
    Try(
      read[Seq[Change]](jsonOrText)
    ).getOrElse(
      unmarshalTextFile(jsonOrText)
    )
  }

  private def unmarshalTextFile(text: String): Seq[Change] = {
    var px = 0
    var py = 0
    text.map {
      case '\n' =>
        val fp = Pointer(py, px, 0, 0)
        py += 1
        px = 0
        val tp = Pointer(py, px, 0, 0)
        Change(EolEnter(false), fp, tp)
      case char =>
        val fp = Pointer(py, px, 0, 0)
        px += 1
        val tp = Pointer(py, px, 0, 0)
        Change(CharInput(char), fp, tp)
    }
  }
}
