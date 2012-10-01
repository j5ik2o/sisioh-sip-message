package org.sisioh.sip.message.impl

import org.sisioh.sip.message.header.impl._
import java.net.InetAddress
import org.sisioh.sip.message.header._
import org.sisioh.sip.message.Request
import scala.Some

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

object SIPRequestBuilder {
  def apply() = new SIPRequestBuilder()
}

class SIPRequestBuilder extends SIPMessageBuilder[SIPRequest, SIPRequestBuilder] {

  var requestLine: Option[RequestLine] = None

  def withRequestLine(requestLine: Option[RequestLine]) = {
    addConfigurator {
      _.requestLine = requestLine
    }
    getThis
  }

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
    builder.withRemoteAddress(vo.remoteAddress)
    builder.withRemotePort(vo.remotePort)
    builder.withLocalAddress(vo.localAddress)
    builder.withLocalPort(vo.localPort)
    builder.withUnrecogizedHeaders(vo.unrecognizedHeaders)
  }

  protected def createValueObject = new SIPRequest(
    requestLine,
    to, from, callId, cSeq, maxForwards, contentLength,
    messageContent,
    remoteAddress, remotePort, localAddress, localPort,
    applicationData,
    unrecognizedHeaders,
    size
  )
}

class SIPRequest
(val requestLine: Option[RequestLine],
 toParam: Option[To],
 fromParam: Option[From],
 callIdParam: Option[CallId],
 cSeqParam: Option[CSeq],
 maxForwardsParam: Option[MaxForwards],

 val contentLength: Option[ContentLength],
 val messageContent: Option[MessageContent],

 val remoteAddress: Option[InetAddress],
 val remotePort: Option[Int],
 val localAddress: Option[InetAddress],
 val localPort: Option[Int],
 val applicationData: Option[Any],
 val unrecognizedHeaders: List[Header],
 val size: Int
  ) extends SIPMessage[Any] {

  type A = SIPRequest
  type B = SIPRequestBuilder

  val isNullRequest: Boolean = false

  def newBuilder = SIPRequestBuilder()


  val forkId = {
    (callId, fromTag) match {
      case (Some(cid), Some(ftag)) =>
        Some((cid.callId + ":" + ftag).toLowerCase)
      case _ => None
    }
  }

  override def encodeAsBytes(transport: String): Array[Byte] = {
    if (isNullRequest) {
      "\r\n\r\n".getBytes
    } else if (requestLine.isEmpty) {
      new Array[Byte](0)
    } else {
      val rlBytes = requestLine.get.encode().getBytes("UTF-8")
      val superBytes = super.encodeAsBytes(transport)
      val retVal = new Array[Byte](rlBytes.size + superBytes.size)
      rlBytes.copyToArray[Byte](retVal, 0, rlBytes.size)
      superBytes.copyToArray[Byte](retVal, rlBytes.size, superBytes.size)
      retVal
    }
  }

  private var nameTable: Map[String, String] = Map.empty

  private def putName(name: String):Unit = nameTable += (name -> name)

  putName(Request.INVITE)
  putName(Request.BYE)
  putName(Request.CANCEL)
  putName(Request.ACK)
  putName(Request.PRACK)
  putName(Request.INFO)
  putName(Request.MESSAGE)
  putName(Request.NOTIFY)
  putName(Request.OPTIONS)
  putName(Request.PRACK)
  putName(Request.PUBLISH)
  putName(Request.REFER)
  putName(Request.REGISTER)
  putName(Request.SUBSCRIBE)
  putName(Request.UPDATE)


  private def getCannonicalName(method: String): Option[String] = {
    if (nameTable.contains(method))
      nameTable.get(method)
    else
      Some(method)
  }

  private def getRequestLineByDefaults: Option[RequestLine] = {
    requestLine.map {
      r =>
        (r.method, cSeq) match {
          case (None, Some(s)) =>
            val method = getCannonicalName(s.method)
            RequestLine(r.uri, method, r.sipVersion)
          case _ =>
            r
        }
    }
  }

  override def encode(builder: StringBuilder): StringBuilder = {
    if (requestLine.isDefined) {
      getRequestLineByDefaults.foreach {
        rl =>
          builder.append(rl.encode())
      }
      super.encode(builder)
    } else if (isNullRequest) {
      builder.append("\r\n\r\n")
    } else {
      super.encode(builder)
    }
  }

  def encodeMessage(sb: StringBuilder) = {
    if (requestLine.isDefined) {
      getRequestLineByDefaults.map {
        _.encode(sb)
      }.get
      encodeSIPHeaders(sb)
    } else if (isNullRequest) {
      sb.append("\r\n\r\n")
    } else
      encodeSIPHeaders(sb)
  }

  toParam.foreach {
    addHeader(_)
  }
  fromParam.foreach {
    addHeader(_)
  }
  callIdParam.foreach {
    addHeader(_)
  }
  cSeqParam.foreach {
    addHeader(_)
  }

  maxForwardsParam.foreach {
    addHeader(_)
  }
  messageContent.foreach {
    mc =>
      addHeader(mc.contentType)
  }


  val to = getHeader(ToHeader.NAME).map(_.asInstanceOf[To])
  val from = getHeader(FromHeader.NAME).map(_.asInstanceOf[From])
  val cSeq = getHeader(CSeqHeader.NAME).map(_.asInstanceOf[CSeq])
  val callId = getHeader(CallIdHeader.NAME).map(_.asInstanceOf[CallId])
  val maxForwards = getHeader(MaxForwardsHeader.NAME).map(_.asInstanceOf[MaxForwards])
  val fromTag = from.flatMap(_.tag)

  def encodeAsJValue() = null

}
