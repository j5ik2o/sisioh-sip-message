package org.sisioh.sip.message.header

trait CallIdHeader extends Header {
  val callId: String
}

object CallIdHeader {
  val NAME = "Call-ID"
}
