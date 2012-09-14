package org.sisioh.sip.message.address

import org.sisioh.sip.message.header.Parameters

trait SipURI extends URI with Parameters {
  val userName: Option[String]
  val userPassword: Option[String]
  val isSecure: Boolean
  val host: String
  val port: Option[Int]

  def removePort: SipURI

  def getHeader(name: String): Option[String]

  def getHeaderNames: Iterator[String]

  val transportHeaderValue: Option[String] = getHeader("transport")

  val ttlHeaderValue: Option[String] = getHeader("ttl")

  val methodHeaderValue: Option[String] = getHeader("method")

  val userHeaderValue: Option[String] = getHeader("user")

  val mAddrHeaderValue: Option[String] = getHeader("maddr")

  val hasLrHeaderValue: Option[String] = getHeader("lr")
}
