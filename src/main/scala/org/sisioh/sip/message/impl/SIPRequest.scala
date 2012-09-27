package org.sisioh.sip.message.impl

import org.sisioh.sip.message.header.impl._
import java.net.InetAddress
import org.sisioh.sip.message.header.Header

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

class SIPRequestBuilder extends SIPMessageBuilder[SIPRequest, SIPRequestBuilder] {
  protected def getThis = this

  protected def newInstance = new SIPRequestBuilder

  protected def apply(vo: SIPRequest, builder: SIPRequestBuilder) {
    builder.withTo(vo.to)
    builder.withFrom(vo.from)
    builder.withCallId(vo.callId)
    builder.withCSeq(vo.cSeq)
    builder.withMaxForwards(vo.maxForwards)
    builder.withContentLength(vo.contentLength)
    builder.withMessageContent(vo.messageContent)
    builder.withMessageContentBytes(vo.messageContentBytes)
    builder.withMessageContentObject(vo.messageContentObject)
    builder.withRemoteAddress(vo.remoteAddress)
    builder.withRemotePort(vo.remotePort)
    builder.withLocalAddress(vo.localAddress)
    builder.withLocalPort(vo.localPort)
    builder.withUnrecogizedHeaders(vo.unrecognizedHeaders)
  }

  protected def createValueObject = new SIPRequest(
    to, from, callId, cSeq, maxForwards, contentLength,
    messageContent, messageContentBytes, messageContentObject,
    remoteAddress, remotePort, localAddress, localPort,
    unrecognizedHeaders
  )
}

class SIPRequest
(val to: Option[To],
 val from: Option[From],
 val callId: Option[CallId],
 val cSeq: Option[CSeq],
 val maxForwards: Option[MaxForwards],
 val contentLength: Option[ContentLength],
 val messageContent: Option[String],
 val messageContentBytes: Option[Array[Byte]],
 val messageContentObject: Option[Any],
 val remoteAddress: Option[InetAddress],
 val remotePort: Option[Int],
 val localAddress: Option[InetAddress],
 val localPort: Option[Int],
 val unrecognizedHeaders: List[Header])

  extends SIPMessage[Any] {

  type A = SIPRequest
  type B = SIPRequestBuilder

  def newBuilder = new SIPRequestBuilder

  val forkId = ""
  val applicationData = ""

  val size = 0
}
