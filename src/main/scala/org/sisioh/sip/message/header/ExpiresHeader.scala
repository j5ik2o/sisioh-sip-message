package org.sisioh.sip.message.header

trait ExpiresHeader {
  val expires: Int
}

object ExpiresHeader {
  val NAME = "Expires"
}