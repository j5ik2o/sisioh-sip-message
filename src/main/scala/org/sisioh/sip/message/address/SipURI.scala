package org.sisioh.sip.message.address

/*
 * Copyright 2012 Sisioh Project and others. (http://www.sisioh.org/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */


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
