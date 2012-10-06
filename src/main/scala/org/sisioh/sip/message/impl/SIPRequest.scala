package org.sisioh.sip.message.impl

import org.sisioh.sip.message.header.impl._
import org.sisioh.sip.message.header._
import impl.ViaList
import org.sisioh.sip.message.{StatusCode, Request}
import org.sisioh.sip.message.address.URI
import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl.GenericURI
import scala.Some
import org.sisioh.sip.util.ParseException
import net.liftweb.json.JsonAST.JField

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

  override protected def apply(vo: SIPRequest, builder: SIPRequestBuilder) {
    super.apply(vo, builder)
    builder.withRequestLine(vo.requestLine)
  }

  protected def createValueObject = new SIPRequest(
    getRequestLine,
    headers.result(),
    messageContent,
    metaData
  )
}

object SIPRequestDecoder extends SIPRequestDecoder

class SIPRequestDecoder extends SIPDecoder[SIPRequest] with SIPRequestParser {

  def decode(source: String) = decodeTarget(source, Request)

}


trait SIPRequestParser extends SIPMessageParser with RequestLineParser {

  lazy val Request: Parser[SIPRequest] = (Request_Line <~ CRLF) ~ (rep(messageHeader) <~ CRLF) ~ opt(messageBody) ^^ {
    case rl ~ mhs ~ mbOpt =>
      SIPRequestBuilder().
        withRequestLine(Some(rl)).
        withHeaders(mhs).
        withMessageContent(mbOpt.map(MessageContent(_))).
        build
  }


}


object SIPRequest {
  val headersToIncludeInResponse = Set(
    FromHeader.NAME.toLowerCase,
    ToHeader.NAME.toLowerCase,
    ViaHeader.NAME.toLowerCase,
    RecordRouteHeader.NAME.toLowerCase,
    CallIdHeader.NAME.toLowerCase,
    CSeqHeader.NAME.toLowerCase,
    TimeStampHeader.NAME.toLowerCase
  )

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

  def apply(requestLine: Option[RequestLine] = None,
            to: Option[To] = None,
            from: Option[From] = None,
            callId: Option[CallId] = None,
            cSeq: Option[CSeq] = None,
            maxForwards: Option[MaxForwards] = None,
            contentLength: Option[ContentLength] = None,
            otherHeaders: List[Header] = List.empty,
            messageContent: Option[MessageContent] = None,
            metaData: Option[MetaData] = None) = {
    val headers = (to :: from :: callId :: cSeq :: maxForwards :: contentLength :: Nil).flatten ++ messageContent.flatMap(_.contentType).toList
    new SIPRequest(
      requestLine,
      headers ++ otherHeaders,
      messageContent,
      metaData
    )
  }

}

class SIPRequest
(val requestLine: Option[RequestLine],
 headersParam: List[Header],
 val messageContent: Option[MessageContent],
 val metaData: Option[MetaData] = None
  ) extends SIPMessage(headersParam) with Request {

  type A = SIPRequest
  type B = SIPRequestBuilder

  messageContent.flatMap(_.contentType).foreach(addHeader)

  val isNullRequest: Boolean = requestLine.isEmpty && headers.isEmpty && messageContent.isEmpty && metaData.isEmpty

  def newBuilder = SIPRequestBuilder()

  val requestURI: Option[URI] = requestLine.map(_.uri)

  val viaHost = getViaHeaders.map(_.getHead.host)

  val viaPort = getViaHeaders.flatMap(_.getHead.port)

  val firstLine = requestLine.map(_.encode())

  val method = requestLine.flatMap(_.method)

  override val sipVersion: Option[String] = requestLine.flatMap(_.sipVersion).orElse(Some(SIPConstants.SIP_VERSION_STRING))

  def createResponse(statusCode: StatusCode.Value): SIPResponse = {
    val reasonPhrase = SIPResponse.getReasonPhrase(statusCode.id)
    createResponse(statusCode, reasonPhrase)
  }

  def createResponse
  (statusCode: StatusCode.Value,
   reasonPhrase: Option[String],
   messageConent: Option[MessageContent] = None,
   headers: List[Header] = List.empty): SIPResponse = {

    val statusLine = StatusLine(statusCode, reasonPhrase)
    SIPResponseBuilder().
      withStatusLine(Some(statusLine)).
      withMessageContent(messageConent).
      withHeaders(headers.map(_.asInstanceOf[SIPHeader])).build
    // TODO Record-Routeからコピーする
  }

  def createCancelRequest: Option[SIPRequest] = {
    if (method != Some(Request.INVITE)) {
      None
    } else {
      val newRL = requestLine.map {
        e =>
          RequestLine(e.uri, Some(Request.CANCEL), e.sipVersion)
      }
      val newCseq = cSeq.map {
        cs =>
          new CSeqBuilder().withMethod(Request.ACK).build(cs)
      }
      val newHeaders = headers.filterNot(e =>
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(CSeqHeader.NAME) ||
          SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ProxyAuthorizationHeader.NAME) ||
          SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ContentTypeHeader.NAME) ||
          SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ContactHeader.NAME) ||
          SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ExpiresHeader.NAME) ||
          SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ViaHeader.NAME)
      ) ++ newCseq.toList

      val viaListOpt = getViaHeaders.map {
        e => ViaList(List(e.headers.head))
      }

      val newHeaderWithVia = viaListOpt.map {
        e =>
          e :: newHeaders
      }.getOrElse(newHeaders)


      Some(new SIPRequest(
        requestLine = newRL,
        headersParam = newHeaderWithVia,
        messageContent = None
      ))
    }

  }

  def createErrorAck(responseToHeader: To): SIPRequest = {
    val newRL = requestLine.map {
      e =>
        RequestLine(e.uri, Some(Request.ACK), e.sipVersion)
    }
    val newHeaders = headers.filterNot(e =>
      SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ToHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(RouteHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ProxyAuthorizationHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ContentTypeHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ContactHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ExpiresHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ViaHeader.NAME)
    ) ++ List(responseToHeader)

    val viaListOpt = getViaHeaders.map {
      e => ViaList(List(e.headers.head))
    }

    val newHeaderWithVia = viaListOpt.map {
      e =>
        e :: newHeaders
    }.getOrElse(newHeaders)


    new SIPRequest(
      requestLine = newRL,
      headersParam = newHeaderWithVia,
      messageContent = None
    )
  }


  def createAckRequest(responseToHeader: To): SIPRequest = {
    val newRL = requestLine.map {
      e =>
        RequestLine(e.uri, Some(Request.ACK), e.sipVersion)
    }
    val newCseq = cSeq.map {
      cs =>
        new CSeqBuilder().withMethod(Request.ACK).build(cs)
    }

    val newHeaders = headers.filterNot(e =>
      SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ToHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(CSeqHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(RouteHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ProxyAuthorizationHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ContentTypeHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ContactHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ExpiresHeader.NAME) ||
        SIPHeaderNamesCache.toLowerCase(e.name) == SIPHeaderNamesCache.toLowerCase(ViaHeader.NAME)
    ) ++ newCseq.toList ++ List(responseToHeader)

    val viaListOpt = getViaHeaders.map {
      e => ViaList(List(e.headers.head))
    }

    val newHeaderWithVia = viaListOpt.map {
      e =>
        e :: newHeaders
    }.getOrElse(newHeaders)


    new SIPRequest(
      requestLine = newRL,
      headersParam = newHeaderWithVia,
      messageContent = None
    )
  }

  override def removeHeader(headerName: String): SIPRequest = {
    removeHeader(headerName, false).asInstanceOf[SIPRequest]
  }


  override def removeContent: SIPRequest = {
    super.removeContent
  }


  override def encodeAsBytes(transport: Option[String] = None): Array[Byte] = {
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


  private def getCannonicalName(method: String): Option[String] = {
    if (SIPRequest.nameTable.contains(method))
      SIPRequest.nameTable.get(method)
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

  def encodeLineAsJField() = {
    JField("requestLine", requestLine.get.encodeAsJValue())
  }


  def validateHeaders: Unit = {
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
      case Some(Request.NOTIFY) if (header(SubscriptionStateHeader.NAME).isEmpty) =>
        throw new ParseException(Some(prefix + SubscriptionStateHeader.NAME))
      case Some(Request.PUBLISH) if (header(EventHeader.NAME).isEmpty) =>
        throw new ParseException(Some(prefix + EventHeader.NAME))
      case _ =>
    }

    // TODO トランザクション状態チェック

    val cSeqMethod = cSeq.map(_.method)
    if (requestLine.isEmpty && method.isEmpty && cSeqMethod.isEmpty && method != cSeqMethod) {
      throw new ParseException(Some("CSEQ method mismatch with  Request-Line "))
    }

  }

  override def equals(obj: Any) = obj match {
    case that: SIPRequest =>
      //      println("requestLine = ", requestLine, that.requestLine, (requestLine == that.requestLine))
      //      println("headers = ", headers, that.headers, (headers == that.headers))
      requestLine == that.requestLine &&
        headers == that.headers
    case _ =>
      false
  }

  override def hashCode = 31 * requestLine.## + 31 * headers.##
}
