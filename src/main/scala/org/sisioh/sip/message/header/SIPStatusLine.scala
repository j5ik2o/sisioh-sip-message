package org.sisioh.sip.message.header

import org.sisioh.sip.message.StatusCode

trait SIPStatusLine {

  val statusCode: StatusCode.Value
  val reasonPhrase: Option[String]
  val sipVersion: Option[String]

  val versionMajor: Option[String]
  val versionMinor: Option[String]

}
