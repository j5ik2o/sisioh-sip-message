package org.sisioh.sip.message.header

import org.sisioh.sip.message.address.URI

trait SIPRequestLine {
  val uri: URI
  val method: Option[String]
  val sipVersion: Option[String]
  val versionMajor: Option[String]
  val versionMinor: Option[String]
}
