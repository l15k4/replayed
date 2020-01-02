package com.pragmaxim.replayed.event

import com.pragmaxim.scalajs.dom.KCode

case object UndoEvent extends EditorEvent {
  val keyCode = KCode.Z
  val char = KCode.letterKeyToLowerCaseCharCode(keyCode)
}

case object RedoEvent extends EditorEvent {
  val keyCode = KCode.Y
  val char = KCode.letterKeyToLowerCaseCharCode(keyCode)
}
