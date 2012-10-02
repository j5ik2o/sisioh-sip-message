package org.sisioh.sip.message.impl

import java.net.InetAddress
import org.sisioh.sip.message.header.impl.{SIPHeader, ContentLength, StatusLine}
import org.sisioh.sip.message.StatusCode
import org.sisioh.sip.message.header._
import scala.Some
import org.sisioh.sip.util.ParseException

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

object SIPResponseBuilder{

  def apply() = new SIPResponseBuilder

}

class SIPResponseBuilder extends SIPMessageBuilder[SIPResponse, SIPResponseBuilder] {

  private var statusLine: Option[StatusLine] = None
  private var isRetransmission = false

  def withStatusLine(statusLine: Option[StatusLine]) = {
    addConfigurator{
      _.statusLine = statusLine
    }
    getThis
  }

  def withIsRetransmission(isRetransmission: Boolean) = {
    addConfigurator{
      _.isRetransmission = isRetransmission
    }
    getThis
  }

  protected def getThis = this

  protected def newInstance = new SIPResponseBuilder

  override protected def apply(vo: SIPResponse, builder: SIPResponseBuilder) {
    super.apply(vo, builder)
    builder.withStatusLine(vo.statusLine)
    builder.withIsRetransmission(vo.isRetransmission)
  }

  protected def createValueObject = SIPResponse(
    statusLine, isRetransmission,
    contentLength, messageContent,
    remoteAddress, remotePort,
    localAddress, localPort,
    applicationData, headers, size)

}

object SIPResponse {

  def apply
  (statusLine: Option[StatusLine],
   isRetransmission: Boolean = false,
   contentLength: Option[ContentLength] = None,
   messageContent: Option[MessageContent] = None,
   remoteAddress: Option[InetAddress] = None,
   remotePort: Option[Int] = None,
   localAddress: Option[InetAddress] = None,
   localPort: Option[Int] = None,
   applicationData: Option[Any] = None,
   headers: List[SIPHeader] = List.empty,
   size: Int = 0) =
    new SIPResponse(
      statusLine, isRetransmission, contentLength,
      messageContent, remoteAddress, remotePort,
      localAddress, localPort, applicationData, headers, size)

  def isFinalResponse(rc: Int) = {
    rc >= 200 && rc < 700
  }

  def getReasonPhrase(rc: Int): Option[String] = {
    StatusCode.values.find(_.id == rc).map(_.toString)
  }

}

class SIPResponse
(val statusLine: Option[StatusLine],
 val isRetransmission: Boolean = false,
 contentLengthParam: Option[ContentLength] = None,
 val messageContent: Option[MessageContent] = None,
 val remoteAddress: Option[InetAddress] = None,
 val remotePort: Option[Int] = None,
 val localAddress: Option[InetAddress] = None,
 val localPort: Option[Int] = None,
 val applicationData: Option[Any] = None,
 headersParam: List[SIPHeader] = List.empty,
 val size: Int = 0)
  extends SIPMessage[Any] {

  type A = SIPResponse
  type B = SIPResponseBuilder

  def contentLength: Option[ContentLength] = (contentLengthParam,
    headersParam.find(_.isInstanceOf[ContentLength]).map(_.asInstanceOf[ContentLength])) match {
    case (Some(clp), _) => Some(clp)
    case (_, Some(clp)) => Some(clp)
    case _ => None
  }

  override def encodeAsBytes(transport: String) = {
    val slBytes = statusLine.get.encode().getBytes("UTF-8")
    val superBytes = super.encodeAsBytes(transport)
    val retVal = new Array[Byte](slBytes.length + superBytes.length)
    slBytes.copyToArray[Byte](retVal, 0, slBytes.length)
    superBytes.copyToArray[Byte](retVal, slBytes.length, superBytes.length)
    retVal
  }

  override def encode(builder: StringBuilder) = {
    builder.append(statusLine.map {
      e =>
        e.encode(builder).result + super.encode(builder).result()
    }.getOrElse(super.encode(builder).result()))
  }

  def encodeMessage(sb: StringBuilder) = {
    if (statusLine.isDefined) {
      statusLine.get.encode(sb)
      super.encodeSIPHeaders(sb)
    } else {
      super.encodeSIPHeaders(sb)
    }
  }

  override def hashCode = 31 * statusLine.## + super.hashCode()

  override def equals(obj: Any) = obj match {
    case that: SIPResponse =>
      statusLine == that.statusLine && super.equals(obj)
    case _ =>
      false
  }

  def newBuilder = new SIPResponseBuilder

  def encodeAsJValue() = null

  val statusCode = statusLine.map(_.statusCode)

  val reasonPhrase = statusLine.map(_.reasonPhrase)

  override val sipVersion: Option[String] = statusLine.flatMap(_.sipVersion)

  val isFinalResponse = statusLine.map(e => SIPResponse.isFinalResponse(e.statusCode))

  headersParam.
    foreach(addHeader)

  messageContent.foreach {
    mc =>
      addHeader(mc.contentType)
  }

  def validateHeaders: Unit = {
    if (cSeq.isEmpty) {
      throw new ParseException(Some(CSeqHeader.NAME + "is missing."))
    }
    if (to.isEmpty) {
      throw new ParseException(Some(ToHeader.NAME + "is missing."))
    }
    if (from.isEmpty) {
      throw new ParseException(Some(FromHeader.NAME + "is missing."))
    }
    if (getViaHeaders.isEmpty) {
      throw new ParseException(Some(ViaHeader.NAME + "is missing."))
    }
    if (callId.isEmpty) {
      throw new ParseException(Some(CallIdHeader.NAME + "is missing."))
    }
    if (statusLine.isEmpty) {
      throw new ParseException(Some("StatusLine is missing."))
    }
    statusCode.foreach {
      sc =>
        if (sc > 699) {
          throw new ParseException(Some("Unknown error code!" + sc))
        }
    }
  }

}
