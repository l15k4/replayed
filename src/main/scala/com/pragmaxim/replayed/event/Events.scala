package com.pragmaxim.replayed.event

import com.pragmaxim.replayed.{Document, Change}

import scala.scalajs.js

trait RxEvent
case object DocLoaded extends RxEvent
trait EditorEvent extends RxEvent
trait NavbarEvent extends RxEvent
case object SaveDoc extends NavbarEvent
case object ShareDoc extends NavbarEvent
case object NewOrLast extends NavbarEvent
case class NewDoc(title: String = "untitled", created: Double = new js.Date().getTime(), data: Option[Seq[Change]]) extends NavbarEvent
case class OpenDoc(doc: Document) extends NavbarEvent
case class DeleteDoc(doc: Document) extends NavbarEvent
case class StoreDoc(doc: Document) extends NavbarEvent
