package org.sisioh.sip.message.address

import org.sisioh.sip.message.header.Parameters

trait SipURI extends URI with Parameters {
  val userName: Option[String]
  val userPassword: Option[String]
  val isSecure: Boolean
  val host: String
  val port: Option[Int]

  def removePort: SipURI

  def getHeader(name: String): Option[Any]

  def getHeaderNames: Iterator[String]

  def getTransport: Option[String] = getHeader(NetObject.TRANSPORT).map(_.asInstanceOf[String])

  def getTtl: Option[Int] = getHeader(NetObject.TTL).map(_.asInstanceOf[Int])

  def getMethod: Option[String] = getHeader(NetObject.METHOD).map(_.asInstanceOf[String])

  def getUser: Option[String] = getHeader(NetObject.USER).map(_.asInstanceOf[String])

  def getMAddr: Option[String] = getHeader(NetObject.MADDR).map(_.asInstanceOf[String])

  def hasLr: Option[Boolean] = getHeader(NetObject.LR).map(_.asInstanceOf[Boolean])
}
