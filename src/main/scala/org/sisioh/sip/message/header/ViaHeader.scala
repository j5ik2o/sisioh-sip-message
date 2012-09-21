package org.sisioh.sip.message.header

trait ViaHeader {
  val host: String
  val port: Option[Int]
  val transport: String
  val protocol: String
  val ttl: Option[Int]
  val maddr: String
  val received: Option[String]
  val branch: Option[String]
  val rPort: Option[Int]
}
object ViaHeader {
  val NAME = "Via"
}
