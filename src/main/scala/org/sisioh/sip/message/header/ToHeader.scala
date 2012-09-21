package org.sisioh.sip.message.header

trait ToHeader extends HeaderAddress with Parameters with Header {
  val tag: Option[String]
}

object ToHeader {
  val NAME = "To"
}
