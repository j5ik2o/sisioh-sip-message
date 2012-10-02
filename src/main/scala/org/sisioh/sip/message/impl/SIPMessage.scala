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
import org.sisioh.sip.util.Utils
import org.sisioh.sip.core.Separators
import org.sisioh.dddbase.core.ValueObjectBuilder
import collection.mutable.ListBuffer
import org.specs2.internal.scalaz.Digit._0

abstract class SIPMessageBuilder[T <: SIPMessage[_], S <: SIPMessageBuilder[T, S]] extends ValueObjectBuilder[T, S] {

  def withHeaders(headers: List[SIPHeader]): S = {
    addConfigurator {
      _.headers = headers
    }
    getThis
  }

  def withFrom(from: Option[From]) = {
    addConfigurator {
      _.from = from
    }
    getThis
  }

  def withTo(to: Option[To]) = {
    addConfigurator {
      _.to = to
    }
    getThis
  }

  def withCSeq(cSeq: Option[CSeq]) = {
    addConfigurator {
      _.cSeq = cSeq
    }
    getThis
  }

  def withCallId(callId: Option[CallId]) = {
    addConfigurator {
      _.callId = callId
    }
    getThis
  }

  def withContentLength(contentLength: Option[ContentLength]) = {
    addConfigurator {
      _.contentLength = contentLength
    }
    getThis
  }

  def withMaxForwards(maxForwards: Option[MaxForwards]) = {
    addConfigurator {
      _.maxForwards = maxForwards
    }
    getThis
  }

  def withSize(size: Int) = {
    addConfigurator {
      _.size = size
    }
    getThis
  }

  def withMessageContent(messageContent: Option[MessageContent]) = {
    addConfigurator {
      _.messageContent = messageContent
    }
    getThis
  }

  def withApplicationData(applicationData: Option[Any]) = {
    addConfigurator {
      _.applicationData = applicationData
    }
    getThis
  }

  def withForkId(forkId: String) = {
    addConfigurator {
      _.forkId = forkId
    }
    getThis
  }

  def withRemoteAddress(remoteAddress: Option[InetAddress]) = {
    addConfigurator {
      _.remoteAddress = remoteAddress
    }
    getThis
  }

  def withRemotePort(remotePort: Option[Int]) = {
    addConfigurator {
      _.remotePort = remotePort
    }
    getThis
  }

  def withLocalAddress(localAddress: Option[InetAddress]) = {
    addConfigurator {
      _.localAddress = localAddress
    }
    getThis
  }

  def withLocalPort(localPort: Option[Int]) = {
    addConfigurator {
      _.localPort = localPort
    }
    getThis
  }


  protected var headers: List[SIPHeader] = List.empty

  protected var from: Option[From] = None
  protected var to: Option[To] = None
  protected var cSeq: Option[CSeq] = None
  protected var callId: Option[CallId] = None
  protected var contentLength: Option[ContentLength] = None
  protected var maxForwards: Option[MaxForwards] = None
  protected var size: Int = 0

  protected var messageContent: Option[MessageContent] = None

  protected var applicationData: Option[Any] = null
  protected var forkId: String = ""

  protected var remoteAddress: Option[InetAddress] = None
  protected var remotePort: Option[Int] = None
  protected var localAddress: Option[InetAddress] = None
  protected var localPort: Option[Int] = None

  protected def apply(vo: T, builder: S) {
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
}


object MessageContent {


  def apply(contentType: ContentType,
            contentAsString: String): MessageContent = MessageContent(contentType, contentAsString.getBytes)

}

case class MessageContent
(contentType: ContentType,
 contentBytes: Array[Byte]) {

  def getContentAsString
  (charset: String = DefaultMessageFactory.defaultContentEncodingCharset): String =
    new String(contentBytes, charset)


}


case class HeaderListMap
(headers: ListBuffer[SIPHeader] = ListBuffer.empty,
 headerTable: scala.collection.mutable.Map[String, SIPHeader] = scala.collection.mutable.Map.empty) {

//  private val headers: ListBuffer[SIPHeader] = ListBuffer.empty
//  private val headerTable: scala.collection.mutable.Map[String, SIPHeader] = scala.collection.mutable.Map.empty

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


trait SIPMessage[T] extends MessageObject with Message with MessageExt {

  val unrecognizedHeaders: List[Header] = List.empty
  protected val headerListMap: HeaderListMap = new HeaderListMap

  def headers = headerListMap.toList

  def to = getHeader(ToHeader.NAME).map(_.asInstanceOf[To])

  def from = getHeader(FromHeader.NAME).map(_.asInstanceOf[From])

  def cSeq = getHeader(CSeqHeader.NAME).map(_.asInstanceOf[CSeq])

  def callId = getHeader(CallIdHeader.NAME).map(_.asInstanceOf[CallId])

  def maxForwards = getHeader(MaxForwardsHeader.NAME).map(_.asInstanceOf[MaxForwards])

  def fromTag = from.flatMap(_.tag)

  def contentLength: Option[ContentLength]

  val messageContent: Option[MessageContent]

  val applicationData: Any
  val size: Int

  val remoteAddress: Option[InetAddress]
  val remotePort: Option[Int]

  val localAddress: Option[InetAddress]
  val localPort: Option[Int]


  def getMessageAsEncodedStrings(): List[String] = {
    headerListMap.toList.flatMap {
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


  def encodeAsBytes(transport: String): Array[Byte] = {
    if (isInstanceOf[SIPRequest] && asInstanceOf[SIPRequest].isNullRequest) {
      return "\r\n\r\n".getBytes
    }

    val soruceVia = getHeader(ViaHeader.NAME).asInstanceOf[Via]
    val topVia = Via(soruceVia.sentBy, Protocol(soruceVia.sentProtocol.protocolName, soruceVia.sentProtocol.protocolVersion, transport))
    headerListMap.addOrUpdate(topVia, false)

    val encoding = new StringBuilder()
    headers.synchronized {
      headers.filterNot(_.isInstanceOf[ContentLength]).foreach {
        header =>
          header.encode(encoding)
      }
    }
    contentLength.foreach {
      e =>
        e.encode(encoding).append(Separators.NEWLINE)
    }
    val content = getRawContent
    content.map {
      e =>
        val msgArray = encoding.result().getBytes(getCharset)
        val retVal = new Array[Byte](msgArray.size + e.size)
        msgArray.copyToArray[Byte](retVal, 0, msgArray.size)
        e.copyToArray[Byte](retVal, msgArray.size, e.size)
        retVal
    }.getOrElse {
      encoding.result().getBytes(getCharset)
    }
  }


  def encodeMessage(sb: StringBuilder): StringBuilder

  def removeContent = {
    newBuilder.
      withMessageContent(None).
      withContentLength(Some(ContentLength(0))).
      build(this.asInstanceOf[A])
  }

  def getHeader(headerName: String) = getHeaderLowerCase(SIPHeaderNamesCache.toLowerCase(headerName))

  def getHeaderLowerCase(lowerCassHeaderName: String) = {
    val headerOpt = headerListMap.get(lowerCassHeaderName)
    // printf("headerOpt = (%s), %s, %s\n", headerTable, lowerCassHeaderName, headerOpt)
    headerOpt.map {
      case l: SIPHeaderList[_, _] =>
        l.getHead.asInstanceOf[Header]
      case h =>
        h.asInstanceOf[Header]
    }
  }

  val CONTENT_TYPE_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ContentTypeHeader.NAME)

  val contentType: Option[ContentType] = getHeaderLowerCase(CONTENT_TYPE_LOWERCASE).map(_.asInstanceOf[ContentType])

  def getSIPHeaderListLowerCase(lowerCaseHeaderName: String): Option[SIPHeader] =
    headerListMap.get(lowerCaseHeaderName)

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


  type A <: SIPMessage[T]
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

  def removeHeader(headerName: String, first: Boolean): SIPMessage[T] = {
    val headerNameLowerCase = SIPHeaderNamesCache.toLowerCase(headerName)
    headerListMap.remove(headerNameLowerCase, first)
    this
  }

  def removeHeader(headerName: String) = {
    removeHeader(headerName, false)
  }

  def getHeaderNames = headers.map(_.name).iterator

  def getHeaders(headerName: String): Iterator[Header] = {
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
      _.encode(builder).append(Separators.NEWLINE)
    }
    messageContent.foreach {
      e =>
        builder.append(e.getContentAsString(getCharset))
    }
    builder
  }

  lazy val CONTENT_DISPOSITION_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ContentDispositionHeader.NAME)

  lazy val contentDispositionHeader = getHeaderLowerCase(CONTENT_DISPOSITION_LOWERCASE).map(_.asInstanceOf[ContentDispositionHeader])

  lazy val CONTENT_LANGUAGE_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ContentLanguageHeader.NAME)

  lazy val contentLanguage = getHeaderLowerCase(CONTENT_LANGUAGE_LOWERCASE).map(_.asInstanceOf[ContentLanguageHeader])

  lazy val CONTENT_ENCODING_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ContentEncodingHeader.NAME)

  lazy val contentEncoding = getHeaderLowerCase(CONTENT_ENCODING_LOWERCASE).map(_.asInstanceOf[ContentEncodingHeader])

  lazy val EXPIRES_LOWERCASE = SIPHeaderNamesCache.toLowerCase(ExpiresHeader.NAME)

  lazy val expires = getHeaderLowerCase(EXPIRES_LOWERCASE).map(_.asInstanceOf[ExpiresHeader])

  private lazy val contentEncodingCharset = DefaultMessageFactory.defaultContentEncodingCharset

  protected final def getCharset = {
    contentType.flatMap {
      ct => ct.charset
    }.getOrElse(contentEncodingCharset)
  }

  def getRawContent = {
    messageContent.map(_.contentBytes)
  }

  def getContent = {
    messageContent.map(_.contentBytes)
  }


  val sipVersion: Option[String] = Some(SIPConstants.SIP_VERSION_STRING)

  override def hashCode = 31 * headerListMap.##

  override def equals(obj: Any) = obj match {
    case that: SIPMessage[_] =>
      headerListMap == that.headerListMap
    case _ =>
      false
  }

  def validateHeaders: Unit
}
