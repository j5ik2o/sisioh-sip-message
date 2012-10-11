package org.sisioh.sip.message.impl

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

import org.sisioh.sip.message.header._
import org.sisioh.sip.message.header.impl._
import org.sisioh.sip.message.{Request, Message}
import java.net.InetAddress
import org.sisioh.sip.util._
import org.sisioh.sip.core.Separators
import org.sisioh.dddbase.core.ValueObjectBuilder
import collection.mutable.ListBuffer
import com.twitter.util.Base64StringEncoder
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonAST.JArray
import net.liftweb.json.JsonAST.JField

abstract class SIPMessageBuilder[T <: SIPMessage, S <: SIPMessageBuilder[T, S]] extends ValueObjectBuilder[T, S] {

  def withHeaders(headers: List[SIPHeader]): S = {
    addConfigurator {
      _.headers ++= headers
    }
    getThis
  }

  def withFrom(from: Option[From]) = {
    addConfigurator {
      _.headers ++= from.toList
    }
    getThis
  }

  def withTo(to: Option[To]) = {
    addConfigurator {
      _.headers ++= to.toList
    }
    getThis
  }

  def withCSeq(cSeq: Option[CSeq]) = {
    addConfigurator {
      _.headers ++= cSeq.toList
    }
    getThis
  }

  def withCallId(callId: Option[CallId]) = {
    addConfigurator {
      _.headers ++= callId.toList
    }
    getThis
  }

  def withContentLength(contentLength: Option[ContentLength]) = {
    addConfigurator {
      _.headers ++= contentLength.toList
    }
    getThis
  }

  def withMaxForwards(maxForwards: Option[MaxForwards]) = {
    addConfigurator {
      _.headers ++= maxForwards.toList
    }
    getThis
  }

  def withMessageContent(messageContent: Option[MessageContent]) = {
    addConfigurator {
      e =>
        e.messageContent = messageContent
    }
    getThis
  }

  def withMetaData(metaData: Option[MetaData]) = {
    addConfigurator {
      e =>
        e.metaData = metaData
    }
    getThis
  }


  protected var headers: ListBuffer[SIPHeader] = ListBuffer.empty


  protected var messageContent: Option[MessageContent] = None


  protected var metaData: Option[MetaData] = None

  protected def apply(vo: T, builder: S) {
    builder.withHeaders(vo.headers)
    builder.withMessageContent(vo.messageContent)
    builder.withMetaData(vo.metaData)
  }
}


object MessageContent {

  def apply
  (contentBytes: Array[Byte]): MessageContent = new MessageContent(contentBytes, None)

  def apply
  (contentBytes: Array[Byte],
   contentType: Option[ContentType]): MessageContent = new MessageContent(contentBytes, contentType)

  def apply
  (contentAsString: String): MessageContent = new MessageContent(contentAsString.getBytes, None)

  def apply
  (contentAsString: String,
   contentType: Option[ContentType]): MessageContent = new MessageContent(contentAsString.getBytes, contentType)

}

class MessageContent
(val contentBytes: Array[Byte],
 val contentType: Option[ContentType] = None) {

  def getContentAsString
  (charset: String = DefaultMessageFactory.defaultContentEncodingCharset): String =
    new String(contentBytes, charset)

  override def hashCode() = 31 * contentBytes.## + 31 * contentType.##

  override def equals(obj: Any) = obj match {
    case that: MessageContent =>
      contentBytes.sameElements(that.contentBytes) &&
        contentType == that.contentType
    case _ =>
      false
  }

  override def toString() = "MessageConent(%s,%s)".format(getContentAsString(), contentType)
}


protected case class HeaderListMap
(headers: ListBuffer[SIPHeader] = ListBuffer.empty,
 headerTable: scala.collection.mutable.Map[String, SIPHeader] = scala.collection.mutable.Map.empty) {

  def toList = headers.result()

  def toMap: scala.collection.immutable.Map[String, SIPHeader] = headerTable.toMap

  def addOrUpdate(header: SIPHeader, first: Boolean) = {
    val headerNameLowerCase = SIPHeaderNamesCache.toLowerCase(header.name)
    if (headerTable.contains(headerNameLowerCase) == false) {
      add(header)
    } else {
      update(header, first)
    }
  }

  def update(header: SIPHeader, first: Boolean) = {
    val headerNameLowerCase = SIPHeaderNamesCache.toLowerCase(header.name)
    if (headerTable.contains(headerNameLowerCase)) {
      header match {
        case shl: SIPHeaderList[_, _] =>
          val hdrList = headerTable.get(headerNameLowerCase).map(_.asInstanceOf[SIPHeaderList[_, SIPHeader]])
          hdrList.map {
            e =>
              val list = header.asInstanceOf[SIPHeaderList[_, _]]
              val newHdrList = e.concatenate(list, first)
              headerTable += (headerNameLowerCase -> newHdrList.asInstanceOf[SIPHeader])
          }.getOrElse {
            headerTable += (headerNameLowerCase -> header)
          }
        case sh: SIPHeader =>
          headerTable += (SIPHeaderNamesCache.toLowerCase(header.name) -> header)
      }

    }
  }

  def contains(key: String) = headerTable.contains(key)

  def get(name: String) = {
    val headerNameLowerCase = SIPHeaderNamesCache.toLowerCase(name)
    headerTable.get(headerNameLowerCase)
  }

  def add(header: SIPHeader): Unit = {
    val headerNameLowerCase = SIPHeaderNamesCache.toLowerCase(header.name)
    if (headerTable.contains(headerNameLowerCase) == false) {
      headers += header
      headerTable += (headerNameLowerCase -> header)
    }
  }

  private def removeInHeaders(name: String) = {
    val headerNameLowerCase = SIPHeaderNamesCache.toLowerCase(name)
    headers.filter(_.name.equalsIgnoreCase(headerNameLowerCase)).foreach {
      e =>
        headers -= e
    }
  }

  def remove(name: String): Unit = remove(name, false)

  def remove(name: String, first: Boolean): Unit = {
    get(name).foreach {
      h =>
        remove(h, first)
    }
  }

  def remove(header: SIPHeader): Unit = remove(header, false)

  def remove(header: SIPHeader, first: Boolean): Unit = {
    val headerNameLowerCase = SIPHeaderNamesCache.toLowerCase(header.name)
    header match {
      case shl: SIPHeaderList[_, _] =>
        val sipHeaderList = headerTable.get(headerNameLowerCase).map(_.asInstanceOf[SIPHeaderList[_, SIPHeader]])
        sipHeaderList.foreach {
          shl =>
            if (shl.isEmpty) {
              removeInHeaders(headerNameLowerCase)
              headerTable -= (headerNameLowerCase)
            } else {
              val newSHL = {
                if (first) shl.removeHead else shl.removeLast
              }.asInstanceOf[SIPHeader]
              headerTable += (headerNameLowerCase -> newSHL)
            }
        }
      case sh: SIPHeader =>
        if (headerTable.contains(headerNameLowerCase)) {
          removeInHeaders(headerNameLowerCase)
          headerTable -= headerNameLowerCase
        }
    }
  }

}

case class MetaData
(remoteAddress: Option[InetAddress],
 remotePort: Option[Int],
 localAddress: Option[InetAddress],
 localPort: Option[Int],
 applicationData: Any)

object SIPMessageDecoder extends SIPMessageDecoder[SIPMessage] with SIPRequestParser with SIPResponseParser {
  protected val message = Request | Response
}


trait SIPMessageDecoder[T] extends SIPDecoder[T] {

  val delimiters = List[Byte]('\r', '\n', '\r', '\n')

  def decode(source: Array[Byte]): T = {
    val index = source.indexOfSlice(delimiters)
    require(index > 0)
    val size = source.size
    val headerEndPos = index + delimiters.size
    val headers = source.slice(0, headerEndPos)
    val sourceAsString = new String(headers, "UTF-8")
    val result = decodeTarget(sourceAsString, message)
    result.contentLength.flatMap {
      cl =>
        if (cl.contentLength > 0 && size - headerEndPos > 0) {
          val contents = source.slice(headerEndPos, headerEndPos + cl.contentLength)
          Some(result.withMessageContent(Some(MessageContent(contents))).asInstanceOf[T])
        } else {
          None
        }
    }.getOrElse(result).asInstanceOf[T]
  }

  def decode(source: String): T = decode(source.getBytes("UTF-8"))

  protected val message: Parser[SIPMessage]
}


trait SIPMessageParser extends ParserBase with CallIdParser
with CSeqParser
with FromParser
with ToParser
with MaxForwardsParser
with UserAgentParser
with ServerParser
with ExpiresParser
with ViaListParser
with ContentTypeParser
with ContentLengthParser {
  //  lazy val messageHeader = (Accept | Accept_Encoding | Accept_Language | Alert_Info |
  //    Allow | Authentication_Info | Authorization | Call_ID |
  //    Call_Info | Contact | Content_Disposition |
  //    Content_Encoding | Content_Language | Content_Length | Content_Type |
  //    CSeq | Date | Error_Info | Expires |
  //    From | In_Reply_To | Max_Forwards |
  //    MIME_Version | Min_Expires | Organization |
  //    Priority | Proxy_Authenticate | Proxy_Authorization |
  //    Proxy_Require | Record_Route | Reply_To |
  //    Require | Retry_After | Route |
  //    Server | Subject | Supported | Timestamp |
  //    To | Unsupported | User_Agent | Via |
  //    Warning | WWW_Authenticate | extensionHeader) ~ CRLF

  lazy val messageHeader: Parser[SIPHeader] = (Call_ID | cseq | expires |
    contentType | Content_Length |
    from | Max_Forwards | SERVER |
    to | USER_AGENT | VIA /*| extensionHeader */) <~ CRLF

  lazy val TEXT_UTF8_TRIM: Parser[String] = rep1sep(TEXT_UTF8char, rep(LWS)) ^^ {
    _.mkString
  }
  lazy val TEXT_UTF8char: Parser[String] = chrRange(0x21, 0x7E) ^^ {
    _.toString
  } | UTF8_NONASCII
  //  lazy val extensionHeader: Parser[Header] = headerName ~ (HCOLON ~> headerValue) ^^ {
  //    case n ~ v =>
  //
  //  }
  lazy val headerName = token
  lazy val headerValue = rep(TEXT_UTF8char | UTF8_CONT | LWS)

  //  lazy val messageBody = rep1( """.""".r) ^^ {
  //    case p =>
  //      p.mkString.getBytes
  //  }

}

trait SIPMessageJsonFieldNames extends JsonFieldNames {
  val CONTENT = "content"
  val HEADERS = "headers"
}

abstract class SIPMessageJsonDecoder[A <: SIPMessage, LINE] extends JsonDecoder[SIPMessage] with SIPMessageJsonFieldNames {

  protected def decodeLine(json: JValue): LINE

  protected def createInstance(line: LINE, sipHeaders: List[SIPHeader], content: Option[Array[Byte]]): A

  def decode(json: JValue) = {
    val JArray(headersAsJArray) = (json \ HEADERS)
    val headers = headersAsJArray.map {
      h =>
        SIPHeaderJsonDecoder.decode(h)
    }
    val contentOpt = (json \ CONTENT).toOpt.map {
      e =>
        Base64StringEncoder.decode(e.asInstanceOf[JString].s)
    }
    createInstance(decodeLine(json), headers, contentOpt)
  }
}

object SIPMessageJsonEncoder extends SIPMessageJsonEncoder

class SIPMessageJsonEncoder extends JsonEncoder[SIPMessage] with SIPMessageJsonFieldNames {

  def encode(model: SIPMessage) = {
    val headersAsJValue = JArray(model.headers.map {
      header =>
        header.encodeAsJValue()
    })
    val messageContentAsJValue = model.messageContent.map {
      e =>
        JField(CONTENT, JString(Base64StringEncoder.encode(e.contentBytes)))
    }
    JObject(
      (Some(model.encodeLineAsJField) ::
        Some(JField(HEADERS, headersAsJValue)) ::
        messageContentAsJValue :: Nil).flatten
    )
  }

}


object SIPMessage {


}


abstract class SIPMessage
(headersParam: List[Header])
  extends MessageObject with Message with MessageExt {

  def firstLine: Option[String]

  protected val headerListMap: HeaderListMap = new HeaderListMap

  headersParam.foreach(addHeader)

  val unrecognizedHeaders: List[Header] = List.empty

  def headers = headerListMap.toList

  def to = header(ToHeader.NAME).map(_.asInstanceOf[To])

  def from = header(FromHeader.NAME).map(_.asInstanceOf[From])

  def cSeq = header(CSeqHeader.NAME).map(_.asInstanceOf[CSeq])

  def callId = header(CallIdHeader.NAME).map(_.asInstanceOf[CallId])

  def maxForwards = header(MaxForwardsHeader.NAME).map(_.asInstanceOf[MaxForwards])

  def fromTag = from.flatMap(_.tag)

  val messageContent: Option[MessageContent]

  def headerSize: Int = headers.size

  val metaData: Option[MetaData]

  val applicationData = metaData.map(_.applicationData)

  def contentLength: Option[ContentLength] =
    headers.find(_.isInstanceOf[ContentLength]).
      map(_.asInstanceOf[ContentLength])

  def withMetaData(metaData: Option[MetaData]) =
    newBuilder.withMetaData(metaData).build(this.asInstanceOf[A])

  def withMessageContent(messageContent: Option[MessageContent]) =
    newBuilder.withMessageContent(messageContent).build(this.asInstanceOf[A])

  def getMessageAsEncodedStrings(): List[String] = {
    headers.flatMap {
      case l: SIPHeaderList[_, _] =>
        l.getHeadersAsEncodedStrings
      case h: SIPHeader =>
        List(h.encode())
    }
  }

  def encodeSIPHeaders(builder: StringBuilder): StringBuilder = {
    headers.filterNot(_.isInstanceOf[ContentLength]).foreach {
      e =>
        e.encode(builder)
    }
    contentLength.map {
      e =>
        e.encode(builder).append(Separators.NEWLINE)
    }
    builder
  }

  def encodeAsBytes(transport: Option[String] = None): Array[Byte] = {
    if (isInstanceOf[SIPRequest] && asInstanceOf[SIPRequest].isNullRequest) {
      return "\r\n\r\n".getBytes
    }
    val soruceVia = header(ViaHeader.NAME).map(_.asInstanceOf[Via])
    val topVia = soruceVia.map {
      svia =>
        Via(svia.sentBy,
          Protocol(svia.sentProtocol.protocolName,
            svia.sentProtocol.protocolVersion, transport.getOrElse(svia.transport)))
    }
    topVia.foreach {
      tv =>
        headerListMap.addOrUpdate(tv, false)
    }
    val encoding = new StringBuilder()
    headers.synchronized {
      headers.filterNot(_.isInstanceOf[ContentLength]).foreach {
        header =>
          header.encode(encoding)
      }
    }
    contentLength.foreach {
      _.encode(encoding)
    }
    encoding.append(Separators.NEWLINE)
    val r = content.map {
      e =>
        val msgArray = encoding.result().getBytes(charset)
        val retVal = new Array[Byte](msgArray.size + e.size)
        msgArray.copyToArray[Byte](retVal, 0, msgArray.size)
        e.copyToArray[Byte](retVal, msgArray.size, e.size)
        retVal
    }.getOrElse {
      encoding.result().getBytes(charset)
    }
    r
  }


  def encodeMessage(sb: StringBuilder): StringBuilder

  def removeContent = {
    newBuilder.
      withMessageContent(None).
      withContentLength(Some(ContentLength(0))).
      build(this.asInstanceOf[A])
  }

  def header(headerName: String) = getHeaderLowerCase(SIPHeaderNamesCache.toLowerCase(headerName))

  def getHeaderLowerCase(lowerCassHeaderName: String) = {
    val headerOpt = headerListMap.get(lowerCassHeaderName)
    headerOpt.map {
      case l: SIPHeaderList[_, _] =>
        l.getHead.asInstanceOf[Header]
      case h =>
        h.asInstanceOf[Header]
    }
  }

  protected lazy val CONTENT_DISPOSITION_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ContentDispositionHeader.NAME)

  protected lazy val CONTENT_LANGUAGE_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ContentLanguageHeader.NAME)

  protected lazy val CONTENT_ENCODING_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ContentEncodingHeader.NAME)

  protected lazy val EXPIRES_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ExpiresHeader.NAME)

  protected lazy val CONTENT_TYPE_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ContentTypeHeader.NAME)

  protected lazy val contentEncodingCharset = DefaultMessageFactory.defaultContentEncodingCharset

  def contentType: Option[ContentType] = getHeaderLowerCase(CONTENT_TYPE_LOWERCASE).map(_.asInstanceOf[ContentType])

  def getSIPHeaderListLowerCase(lowerCaseHeaderName: String): Option[SIPHeader] = headerListMap.get(lowerCaseHeaderName)

  def getViaHeaders = getSIPHeaderListLowerCase("via").map(_.asInstanceOf[ViaList])

  def getViaHeadHeader = getViaHeaders.map(_.getHead)

  def getDialogId(isServer: Boolean): Option[String] = getDialogId(isServer, None)

  def getDialogId(isServer: Boolean, toTag: Option[String]): Option[String] = {
    val toAndFromTags = (from.flatMap(_.tag) :: to.flatMap(_.tag).orElse(toTag) :: Nil).flatten
    val r = if (isServer) {
      toAndFromTags.reverse
    } else {
      toAndFromTags
    }
    callId.map(e => (e.callId :: r).mkString(Separators.COLON))
  }

  def getTransactionId: String = {
    val topVia = getViaHeadHeader
    if (topVia.isDefined &&
      topVia.get.branch.isDefined &&
      topVia.get.branch.get.toUpperCase.startsWith(SIPConstants.BRANCH_MAGIC_COOKIE_UPPER_CASE)) {
      if (cSeq.get.method == Request.CANCEL) {
        topVia.get.branch.get + ":" + cSeq.get.method.toLowerCase
      } else {
        topVia.get.branch.get.toLowerCase
      }
    } else {

      val retVal =
        from.flatMap {
          _.tag.map {
            tag =>
              new StringBuilder().append(tag).append("-")
          }
        }.getOrElse(new StringBuilder)

      val cid = callId.get.callId
      retVal.append(cid).append("-")
      retVal.append(cSeq.get.sequenceNumber).append("-").append(cSeq.get.method)
      if (topVia.isDefined) {
        retVal.append("-").append(topVia.get.sentBy.encode())
        if (topVia.get.sentBy.port.isDefined) {
          retVal.append("-").append(5060)
        }
      }
      if (cSeq.get.method == Request.CANCEL) {
        retVal.append(Request.CANCEL)
      }
      retVal.result().toLowerCase.replace(":", "-").replace("@", "-") + Utils.signature
    }
  }


  def getForkId = {
    (callId, fromTag) match {
      case (Some(cid), Some(ftag)) =>
        Some((cid.callId + ":" + ftag).toLowerCase)
      case _ => None
    }
  }


  def newBuilder: SIPMessageBuilder[A, B]


  type A <: SIPMessage
  type B <: SIPMessageBuilder[A, B]

  def addHeader(header: Header) = {
    addLast(header)
  }

  protected def attachHeader(header: Header, first: Boolean = false) = {
    val sipHeader = header.asInstanceOf[SIPHeader]

    val targetHeader: SIPHeader =
      if (SIPHeaderListMapping.hasList(sipHeader)) {
        SIPHeaderListMapping.getList(sipHeader).get
      } else {
        sipHeader
      }
    headerListMap.addOrUpdate(targetHeader, first)
    this
  }


  def addLast(header: Header) = {
    attachHeader(header)
  }

  def addFirst(header: Header) = {
    attachHeader(header, false)
    this
  }

  def removeFirst(headerName: String) =
    removeHeader(headerName, true)

  def removeLast(headerName: String) =
    removeHeader(headerName)

  def removeHeader(headerName: String, first: Boolean): SIPMessage = {
    val headerNameLowerCase = SIPHeaderNamesCache.toLowerCase(headerName)
    headerListMap.remove(headerNameLowerCase, first)
    this
  }

  def removeHeader(headerName: String) = {
    removeHeader(headerName, false)
  }

  def headerNames = headers.map(_.name).iterator

  def headers(headerName: String): Iterator[Header] = {
    val sipHeader = headerListMap.get(SIPHeaderNamesCache.toLowerCase(headerName))
    sipHeader.map {
      e =>
        e match {
          case l: SIPHeaderList[_, _] =>
            l.toList.map(_.asInstanceOf[Header]).iterator
          case _ =>
            List(e.asInstanceOf[Header]).iterator
        }
    }.getOrElse {
      List.empty.iterator
    }
  }

  def encode(builder: StringBuilder): StringBuilder = {
    headers.filterNot(_.isInstanceOf[ContentLength]).foreach {
      _.encode(builder)
    }
    unrecognizedHeaders.foreach {
      header =>
        builder.append(header).append(Separators.NEWLINE)
    }
    contentLength.foreach {
      _.encode(builder)
    }
    builder.append(Separators.NEWLINE)
    messageContent.foreach {
      e =>
        builder.append(e.getContentAsString(charset))
    }
    builder
  }


  def contentDispositionHeader = getHeaderLowerCase(CONTENT_DISPOSITION_LOWERCASE).map(_.asInstanceOf[ContentDispositionHeader])

  def contentLanguage = getHeaderLowerCase(CONTENT_LANGUAGE_LOWERCASE).map(_.asInstanceOf[ContentLanguageHeader])

  def contentEncoding = getHeaderLowerCase(CONTENT_ENCODING_LOWERCASE).map(_.asInstanceOf[ContentEncodingHeader])

  def expires = getHeaderLowerCase(EXPIRES_LOWERCASE).map(_.asInstanceOf[ExpiresHeader])

  protected final def charset = {
    contentType.flatMap {
      ct => ct.charset
    }.getOrElse(contentEncodingCharset)
  }

  def content = messageContent.map(_.contentBytes)

  def contentAsString = messageContent.map {
    e =>
      new String(e.contentBytes, charset)
  }

  val sipVersion: Option[String] = Some(SIPConstants.SIP_VERSION_STRING)

  override def hashCode = 31 * headerListMap.##

  override def equals(obj: Any) = obj match {
    case that: SIPMessage =>
      headers == that.headers &&
        messageContent == that.messageContent
    case _ =>
      false
  }

  def validateHeaders: Unit

  def encodeLineAsJField: JField

  def encodeAsJValue() = SIPMessageJsonEncoder.encode(this)

}


