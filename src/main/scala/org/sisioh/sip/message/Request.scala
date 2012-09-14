package org.sisioh.sip.message

import address.URI

trait Request[T] extends Message[T] {
  val method: String
  val requestURI: URI
}

object Request {
  val ACK = "ACK"
  val BYE = "BYE"
  val CANCEL = "CANCEL"
  val INVITE = "INVITE"
  val OPTIONS = "OPTIONS"
  val REGISTER = "REGISTER"
  val NOTIFY = "NOTIFY"
  val SUBSCRIBE = "SUBSCRIBE"
  val MESSAGE = "MESSAGE"
  val REFER = "REFER"
  val INFO = "INFO"
  val PRACK = "PRACK"
  val UPDATE = "UPDATE"
  val PUBLISH = "PUBLISH"
}
