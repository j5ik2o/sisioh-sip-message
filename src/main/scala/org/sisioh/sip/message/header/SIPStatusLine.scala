package org.sisioh.sip.message.header

trait SIPStatusLine {

  val statusCode: Int
  val reasonPhrase: Option[String]
  val sipVersion: Option[String]

  val versionMajor: Option[String]
  val versionMinor: Option[String]

}
