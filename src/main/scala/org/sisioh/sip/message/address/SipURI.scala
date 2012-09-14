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

  def getTransport: Option[String] = getHeader("transport")

  def getTtl: Option[String] = getHeader("ttl")

  def getMethod: Option[String] = getHeader("method")

  def getUser: Option[String] = getHeader("user")

  def getMAddr: Option[String] = getHeader("maddr")

  def hasLr: Option[String] = getHeader("lr")
}
