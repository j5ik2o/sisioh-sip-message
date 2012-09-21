package org.sisioh.sip.message.address

import org.sisioh.sip.message.header.Parameters

trait TelURL extends URI with Parameters {
  val isGlobal: Boolean
  val postDial: Option[String]
  val hasPostDial: Boolean
  val isdnSubAddress: Option[String]
  val hasIsdnSubAddress: Boolean
  val phoneNumber: String
  val phoneContext: Option[String]
}
