package org.sisioh.sip.message.impl

import org.sisioh.sip.message.header.impl._
import java.net.InetAddress
import org.sisioh.sip.message.header._
import org.sisioh.sip.message.Request
import scala.Some
import org.sisioh.sip.message.address.URI
import org.sisioh.sip.util.ParseException
import org.sisioh.sip.message.address.impl.GenericURI

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

  private val requstLineBuilder = RequestLineBuilder()
  private var isDefinedRequestLine = false

  private def getRequestLine = {
    if (isDefinedRequestLine) Some(requstLineBuilder.build) else None
  }

  def withRequestLine(requestLine: Option[RequestLine]) = {
    addConfigurator {
      e =>
        e.requstLineBuilder.
          withMethod(requestLine.flatMap(_.method)).
          withUri(requestLine.map(_.uri).get).
          withSipVersion(requestLine.flatMap(_.sipVersion))
        e.isDefinedRequestLine = true
    }
    isDefinedRequestLine = true
    getThis
  }

  def withMethod(method: Option[String]) = {
    addConfigurator {
      e =>
        e.requstLineBuilder.withMethod(method)
        e.isDefinedRequestLine = true
    }
    getThis
  }

  def withUri(uri: GenericURI) = {
    addConfigurator {
      e =>
        e.requstLineBuilder.withUri(uri)
        e.isDefinedRequestLine = true
    }
    getThis
  }

  def withSipVersion(sipVersion: Option[String]) = {
    addConfigurator {
      e =>
        e.requstLineBuilder.withSipVersion(sipVersion)
        e.isDefinedRequestLine = true
    }
    getThis
  }

  protected def getThis = this

  protected def newInstance = new SIPRequestBuilder

  protected def apply(vo: SIPRequest, builder: SIPRequestBuilder) {
    builder.withRequestLine(vo.requestLine)
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
    builder.withHeaders(vo.headers)
  }

  protected def createValueObject = new SIPRequest(
    getRequestLine,
    to, from,
    callId, cSeq, maxForwards,
    contentLength,
    messageContent,
    remoteAddress, remotePort,
    localAddress, localPort,
    applicationData,
    headers,
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

 contentLengthParam: Option[ContentLength],
 val messageContent: Option[MessageContent],

 val remoteAddress: Option[InetAddress],
 val remotePort: Option[Int],
 val localAddress: Option[InetAddress],
 val localPort: Option[Int],
 val applicationData: Option[Any],
 headersParam: List[SIPHeader],
 val size: Int
  ) extends SIPMessage[Any] {

  type A = SIPRequest
  type B = SIPRequestBuilder

  val isNullRequest: Boolean = false

  def newBuilder = SIPRequestBuilder()

  val requestURI: Option[URI] = requestLine.map(_.uri)

  val viaHost = getViaHeaders.map(_.getHead.host)

  val viaPort = getViaHeaders.map(_.getHead.port)

  val firstLine = requestLine.map(_.encode())

  val method = requestLine.flatMap(_.method)

  override val sipVersion = requestLine.flatMap(_.sipVersion).getOrElse(SIPConstants.SIP_VERSION_STRING)

  def createAckRequest(responseToHeader: To) = {
    val newRL = requestLine.map {
      e =>
        RequestLine(e.uri, Some(Request.ACK), e.sipVersion)
    }
    val newCseq = cSeq.map {
      cs =>
        new CSeqBuilder().withMethod(Request.ACK).build(cs)
    }
    val newHeaders = headers.filterNot(e =>
      SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(RouteHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ProxyAuthorizationHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ContentTypeHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ContactHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ExpiresHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ViaHeader.NAME)
    )
    val viaListOpt = getViaHeaders.map {
      e => ViaList(List(e.headers.head))
    }

    val newHeaderWithVia = viaListOpt.map {
      e =>
        e :: newHeaders
    }.getOrElse(newHeaders)

    new SIPRequest(
      requestLine = newRL,
      toParam = Some(responseToHeader),
      fromParam = from,
      callIdParam = callId,
      cSeqParam = newCseq,
      maxForwardsParam = maxForwards,
      contentLengthParam = Some(ContentLength(0)),
      messageContent = None,
      remoteAddress = None,
      remotePort = None,
      localAddress = None,
      localPort = None,
      applicationData = None,
      headersParam = newHeaderWithVia,
      size = size
    )

  }

  override def removeHeader(headerName: String): SIPRequest = {
    removeHeader(headerName, false).asInstanceOf[SIPRequest]
  }


  override def removeContent: SIPRequest = {
    super.removeContent
  }

  def forkId = {
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

  private def putName(name: String): Unit = nameTable += (name -> name)

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

  private val newHeadersParam = headersParam.filterNot {
    e =>
      SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ToHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(FromHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(CallIdHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(CSeqHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(MaxForwardsHeader.NAME)
  }

  newHeadersParam.
    filter(e => SIPHeaderNamesCache.toLowerCase(e.name) == ViaHeader.NAME.toLowerCase).
    foreach(addHeader)

  toParam.foreach(addHeader)
  fromParam.foreach(addHeader)
  callIdParam.foreach(addHeader)
  cSeqParam.foreach(addHeader)
  maxForwardsParam.foreach(addHeader)

  messageContent.foreach {
    mc =>
      addHeader(mc.contentType)
  }

  newHeadersParam.
    filterNot(e => SIPHeaderNamesCache.toLowerCase(e.name) == ViaHeader.NAME.toLowerCase).
    foreach(addHeader)

  def headers = headerListMap.toList

  def to = getHeader(ToHeader.NAME).map(_.asInstanceOf[To])

  def from = getHeader(FromHeader.NAME).map(_.asInstanceOf[From])

  def cSeq = getHeader(CSeqHeader.NAME).map(_.asInstanceOf[CSeq])

  def callId = getHeader(CallIdHeader.NAME).map(_.asInstanceOf[CallId])

  def maxForwards = getHeader(MaxForwardsHeader.NAME).map(_.asInstanceOf[MaxForwards])

  def fromTag = from.flatMap(_.tag)

  def encodeAsJValue() = null

  def contentLength: Option[ContentLength] = (contentLengthParam,
    headersParam.find(_.isInstanceOf[ContentLength]).map(_.asInstanceOf[ContentLength])) match {
    case (Some(clp), _) => Some(clp)
    case (_, Some(clp)) => Some(clp)
    case _ => None
  }

  def vaildateHeaders = {
    val prefix = "Missing a required header : "
    if (cSeq.isEmpty) {
      throw new ParseException(Some(prefix + CSeqHeader.NAME))
    }
    if (to.isEmpty) {
      throw new ParseException(Some(prefix + ToHeader.NAME))
    }
    callId match {
      case None =>
        throw new ParseException(Some(prefix + CallIdHeader.NAME))
      case Some(cid) if (cid.callId.isEmpty) =>
        throw new ParseException(Some(prefix + CallIdHeader.NAME))
      case _ =>
    }
    if (from.isEmpty) {
      throw new ParseException(Some(prefix + FromHeader.NAME))
    }
    if (getViaHeaders.isEmpty) {
      throw new ParseException(Some(prefix + ViaHeader.NAME))
    }
    if (maxForwards.isEmpty) {
      throw new ParseException(Some(prefix + MaxForwardsHeader.NAME))
    }
    if (getViaHeadHeader.isEmpty) {
      throw new ParseException(Some("No via header in request!"))
    }
    method match {
      case Some(Request.NOTIFY) if (getHeader(SubscriptionStateHeader.NAME).isEmpty) =>
        throw new ParseException(Some(prefix + SubscriptionStateHeader.NAME))
      case Some(Request.PUBLISH) if (getHeader(EventHeader.NAME).isEmpty) =>
        throw new ParseException(Some(prefix + EventHeader.NAME))
      case _ =>
    }

    // TODO トランザクション状態チェック

    val cSeqMethod = cSeq.map(_.method)
    if (requestLine.isEmpty && method.isEmpty && cSeqMethod.isEmpty && method != cSeqMethod) {
      throw new ParseException(Some("CSEQ method mismatch with  Request-Line "))
    }

  }

}
