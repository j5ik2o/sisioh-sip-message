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

abstract class SIPMessageBuilder[T <: SIPMessage[_], S <: SIPMessageBuilder[T, S]] extends ValueObjectBuilder[T, S] {

  def withUnrecogizedHeaders(unrecognizedHeaders: List[Header]): S = {
    addConfigurator {
      _.unrecognizedHeaders = unrecognizedHeaders
    }
    getThis
  }

  def withHeaders(headers: List[SIPHeader]): S = {
    addConfigurator {
      _.headers = headers
    }
    getThis
  }

  def withHeaderTable(headerTable: Map[String, SIPHeader]) = {
    addConfigurator {
      _.headerTable = headerTable
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


  var unrecognizedHeaders: List[Header] = List.empty
  var headers: List[SIPHeader] = List.empty
  var headerTable: Map[String, SIPHeader] = Map.empty

  var from: Option[From] = None
  var to: Option[To] = None
  var cSeq: Option[CSeq] = None
  var callId: Option[CallId] = None
  var contentLength: Option[ContentLength] = None
  var maxForwards: Option[MaxForwards] = None
  var size: Int = 0

  var messageContent: Option[MessageContent] = None

  var applicationData: Option[Any] = null
  var forkId: String = ""

  var remoteAddress: Option[InetAddress] = None
  var remotePort: Option[Int] = None
  var localAddress: Option[InetAddress] = None
  var localPort: Option[Int] = None


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


trait SIPMessage[T] extends MessageObject with Message with MessageExt {

  import scala.collection.mutable._


  val headers: ListBuffer[SIPHeader] = ListBuffer.empty
  val headerTable: Map[String, SIPHeader] = Map.empty

  val from: Option[From]
  val to: Option[To]
  val cSeq: Option[CSeq]
  val callId: Option[CallId]
  val maxForwards: Option[MaxForwards]
  val contentLength: Option[ContentLength]

  val forkId: Option[String]

  val messageContent: Option[MessageContent]

  val applicationData: Any
  val size: Int

  val remoteAddress: Option[InetAddress]
  val remotePort: Option[Int]

  val localAddress: Option[InetAddress]
  val localPort: Option[Int]


  def getMessageAsEncodedStrings(): List[String] = {
    headers.result().flatMap {
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
    // TODO topViaを更新する

    val encoding = new StringBuilder()
    headers.synchronized {
      headers.filterNot(_.isInstanceOf[ContentLength]).foreach {
        header =>
          header.encode(encoding)
      }
    }
    contentLength.get.encode(encoding)
    encoding.append(Separators.NEWLINE)

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
    val headerOpt = headerTable.get(lowerCassHeaderName)
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
    headerTable.get(lowerCaseHeaderName)

  def getViaHeaders = getSIPHeaderListLowerCase("via").map(_.asInstanceOf[ViaList])

  def getViaHeadHeader = getViaHeaders.map(_.getHead)

  def getDialogId(isServer: Boolean, toTag: Option[String]) = {
    val toAndFromTags = List(
      from.get.tag.toList.flatMap {
        tg =>
          List(Separators.COLON, tg)
      },
      toTag.toList.flatMap {
        tg =>
          List(Separators.COLON, tg)
      }
    ).flatten
    val r = if (isServer) {
      toAndFromTags.reverse
    } else {
      toAndFromTags
    }
    val rr = callId.get.callId :: r
    rr.mkString
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
        from.get.tag.map {
          tag =>
            new StringBuilder().append(tag).append("-")
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

  override def hashCode() = 31 * callId.##

  //  def newBuilder[A <: SIPMessage[T], B <: SIPMessageBuilder[A, B]]: SIPMessageBuilder[A, B]
  def newBuilder: SIPMessageBuilder[A, B]


  type A <: SIPMessage[T]
  type B <: SIPMessageBuilder[A, B]

  def addHeader(header: Header) = {
    addLast(header)
  }

  def attachHeader(header: Header, replace: Boolean = false, first: Boolean = false) = {
    val headerNameLowerCase = header.name.toLowerCase
    val sipHeader = header.asInstanceOf[SIPHeader]

    val targetHeader: SIPHeader = if (SIPHeaderListMapping.hasList(sipHeader) &&
      classOf[SIPHeaderList[_,_]].isAssignableFrom(sipHeader.getClass) == false) {
      SIPHeaderListMapping.getList(sipHeader).get
    } else {
      sipHeader
    }

    if (replace) {
      headerTable -= (headerNameLowerCase)
    } else if (headerTable.contains(headerNameLowerCase) && targetHeader.isInstanceOf[SIPHeaderList[_,_]] == false) {
      if (targetHeader.isInstanceOf[ContentLength]) {
        val contentLength = targetHeader.asInstanceOf[ContentLength]
        // 更新
      }
    }

    // 更新系
    if (headerTable.contains(headerNameLowerCase) == false) {
      headerTable += (headerNameLowerCase -> targetHeader)
      headers += targetHeader
      // 追加系
    } else {
      if (targetHeader.isInstanceOf[SIPHeaderList[_, _]]) {
        val hdrList = headerTable.get(headerNameLowerCase).map(_.asInstanceOf[SIPHeaderList[_, SIPHeader]])
        hdrList.map {
          e =>
            val list = sipHeader.asInstanceOf[SIPHeaderList[_, _]]
            val newHdrList = e.concatenate(list, false)
            headerTable += (headerNameLowerCase -> newHdrList.asInstanceOf[SIPHeader])
        }.getOrElse {
          headerTable += (headerNameLowerCase -> targetHeader)
        }
      } else {
        headerTable + (headerNameLowerCase -> targetHeader)
      }
    }
    this
  }


  def addLast(header: Header) = {
    attachHeader(header)
  }

  def addFirst(header: Header) = {
    headers.prepend(header.asInstanceOf[SIPHeader])
    this
  }

  def removeFirst(headerName: String) =
    removeHeader(headerName, true)

  def removeLast(headerName: String) =
    removeHeader(headerName)

  def removeHeader(headerName: String, first: Boolean = false): SIPMessage[T] = {
    val headerNameLowerCase = SIPHeaderNamesCache.toLowerCase(headerName)
    val toRemove = headerTable.get(headerNameLowerCase)
    toRemove.foreach {
      h =>
        h match {
          case sipHeaderList: SIPHeaderList[_, _] =>
            val newSHL = (if (first) sipHeaderList.removeHead else sipHeaderList.removeLast).asInstanceOf[SIPHeader]
            headerTable += (headerNameLowerCase -> newSHL)

            if (sipHeaderList.isEmpty) {
              headers.filter(_.name.equalsIgnoreCase(headerNameLowerCase)).foreach {
                e =>
                  headers -= e
              }
              headerTable -= (headerNameLowerCase)
            }
          case _ =>
            headerTable -= (headerNameLowerCase)
            //            h match {
            //              case h: From =>
            //                builder.withFrom(None)
            //              case h: To =>
            //                builder.withTo(None)
            //              case h: CSeq =>
            //                builder.withCSeq(None)
            //              case h: MaxForwards =>
            //                builder.withMaxForwards(None)
            //              case h: CallId =>
            //                builder.withCallId(None)
            //              case h: ContentLength =>
            //                builder.withContentLength(None)
            //            }
            headers.filter(_.name.equalsIgnoreCase(headerNameLowerCase)).foreach {
              e =>
                headers -= e
            }
        }
    }
    this
  }

  def removeHeader(headerName: String) = {
    null
  }

  def getHeaderNames = headers.map(_.name).iterator

  def getHeaders(headerName: String): Iterator[Header] = {
    val sipHeader = headerTable.get(SIPHeaderNamesCache.toLowerCase(headerName))
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

  //  def encodeByJson(builder: StringBuilder) = null

  //  def removeFirst(headerName: String) = null

  //  def removeLast(headerName: String) = null

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


  val sipVersion = SIPConstants.SIP_VERSION_STRING
}
