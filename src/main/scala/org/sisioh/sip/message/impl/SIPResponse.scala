package org.sisioh.sip.message.impl

import org.sisioh.sip.message.{Response, StatusCode}
import org.sisioh.sip.message.header._
import impl._
import org.sisioh.sip.util.{SIPDecoder, ParseException}
import net.liftweb.json.JsonAST.{JValue, JField}

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

object SIPResponseBuilder {

  def apply() = new SIPResponseBuilder

}

class SIPResponseBuilder extends SIPMessageBuilder[SIPResponse, SIPResponseBuilder] {

  private val statusLineBuilder = StatusLineBuilder()
  private var isDefinedStatusLine = false

  private var isRetransmission = false

  def withStatusCode(statusCode: StatusCode.Value) = {
    addConfigurator {
      e =>
        e.statusLineBuilder.withStatusCode(statusCode)
        e.isDefinedStatusLine = true
    }
    getThis
  }

  def withReasonPhrase(reasonPhrase: Option[String]) = {
    addConfigurator {
      e =>
        e.statusLineBuilder.withReasonPhrase(reasonPhrase)
        e.isDefinedStatusLine = true
    }
    getThis
  }

  def withSipVersion(sipVersion: Option[String]) = {
    addConfigurator {
      e =>
        e.statusLineBuilder.withSipVersion(sipVersion)
        e.isDefinedStatusLine = true
    }
    getThis
  }


  def withStatusLine(statusLine: Option[StatusLine]) = {
    addConfigurator {
      e =>
        e.statusLineBuilder.
          withStatusCode(statusLine.map(_.statusCode).get).
          withReasonPhrase(statusLine.flatMap(_.reasonPhrase)).
          withSipVersion(statusLine.flatMap(_.sipVersion))
        e.isDefinedStatusLine = true
    }
    getThis
  }

  def withIsRetransmission(isRetransmission: Boolean) = {
    addConfigurator {
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

  private def getStatusLine = {
    if (isDefinedStatusLine) Some(statusLineBuilder.build) else None
  }

  protected def createValueObject = new SIPResponse(
    getStatusLine,
    isRetransmission,
    headers.result(),
    messageContent,
    metaData
  )

}

object SIPResponseDecoder extends SIPResponseDecoder

class SIPResponseDecoder extends SIPMessageDecoder[SIPResponse] with SIPResponseParser {

  protected val message = Response

}

trait SIPResponseParser extends SIPMessageParser with StatusLineParser {

  lazy val Response = (Status_Line <~ CRLF) ~ (rep(messageHeader) <~ CRLF) ^^ {
    case st ~ mhs =>
      SIPResponseBuilder().
        withStatusLine(Some(st)).
        withHeaders(mhs).
        build
  }

}

trait SIPResponseJsonFieldNames extends SIPMessageJsonFieldNames {
  val STATUS_LINE = "requestLine"
}

object SIPResponseJsonDecoder extends SIPMessageJsonDecoder[SIPResponse, StatusLine] with SIPResponseJsonFieldNames {

  protected def decodeLine(json: JValue) =
    StatusLineJsonDecoder.decode(json \ STATUS_LINE)

  protected def createInstance(line: StatusLine, sipHeaders: List[SIPHeader], content: Option[Array[Byte]]) = {
    SIPResponse(statusLine = Some(line), headers = sipHeaders, messageContent = content.map(e => MessageContent(e)))
  }

}


object SIPResponse {

  def apply
  (statusLine: Option[StatusLine],
   isRetransmission: Boolean = false,
   headers: List[SIPHeader] = List.empty,
   messageContent: Option[MessageContent] = None,
   metaData: Option[MetaData] = None) =
    new SIPResponse(statusLine, isRetransmission, headers, messageContent, metaData)

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
 headersParam: List[SIPHeader] = List.empty,
 val messageContent: Option[MessageContent] = None,
 val metaData: Option[MetaData])
  extends SIPMessage(headersParam) with Response {

  type A = SIPResponse
  type B = SIPResponseBuilder

  override def encodeAsBytes(transport: Option[String] = None) = {
    val slBytes = statusLine.get.encode().getBytes("UTF-8")
    val superBytes = super.encodeAsBytes(transport)
    val retVal = new Array[Byte](slBytes.length + superBytes.length)
    slBytes.copyToArray[Byte](retVal, 0, slBytes.length)
    superBytes.copyToArray[Byte](retVal, slBytes.length, superBytes.length)
    retVal
  }

  override def encode(builder: StringBuilder) = {
    if (statusLine.isDefined) {
      statusLine.foreach {
        rl =>
          builder.append(rl.encode())
      }
      super.encode(builder)
    } else {
      super.encode(builder)
    }
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
      statusLine == that.statusLine &&
        super.equals(that)
    case _ =>
      false
  }

  def newBuilder = new SIPResponseBuilder

  def encodeLineAsJField() = {
    JField("statusLine", statusLine.get.encodeAsJValue())
  }

  val statusCode = statusLine.map(_.statusCode)

  val reasonPhrase = statusLine.flatMap(_.reasonPhrase)

  override val sipVersion: Option[String] = statusLine.flatMap(_.sipVersion)

  val isFinalResponse = {
    statusLine.map {
      e =>
        SIPResponse.isFinalResponse(e.statusCode.id)
    }
  }

  headersParam.
    foreach(addHeader)

  for {mc <- messageContent
       ct <- mc.contentType} {
    addHeader(ct)
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
        if (sc.id > 699) {
          throw new ParseException(Some("Unknown error code!" + sc))
        }
    }
  }

  def firstLine = statusLine.map(_.encode())

}
