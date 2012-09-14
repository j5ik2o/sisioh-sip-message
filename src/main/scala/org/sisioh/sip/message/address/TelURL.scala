package org.sisioh.sip.message.address

import org.sisioh.sip.message.header.Parameters

trait TelURL extends URI with Parameters {
  val isGlobal: Boolean
  val postDial: Option[String]
  val phoneNumber: String
  val isdnSubAddress: Option[String]
  val phoneContext: Option[String]
}
