package org.sisioh.sip.message.header

trait ContentLengthHeader extends Header {
  val contentLength: Int
}

object ContentLengthHeader {
  val NAME = "Content-Length"
}
