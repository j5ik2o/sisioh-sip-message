package org.sisioh.sip.message.header

trait ToHeader extends HeaderAddress with Parameters with Header {
  val tag: String
}

object ToHeader {
  val NAME = "To"
}
