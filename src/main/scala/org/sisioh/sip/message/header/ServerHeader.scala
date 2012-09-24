package org.sisioh.sip.message.header

trait ServerHeader extends Header {
  val producs: List[String]
}

object ServerHeader {
  val NAME = "Server"
}
