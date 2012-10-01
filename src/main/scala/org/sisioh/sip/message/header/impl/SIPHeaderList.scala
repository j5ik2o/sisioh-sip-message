package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.{SIPHeaderNames, Header}
import org.sisioh.sip.core.Separators
import collection.immutable.List
import net.liftweb.json._


abstract class SIPHeaderList[A <: SIPHeaderList[A, HDR], HDR <: SIPHeader]
(val clazz: Class[_],
 val headerName: String,
 val prettyEncode: Boolean = false,
 private val headers: List[HDR] = List.empty[HDR])
  extends SIPHeader with Header {

  override def toString = encode()

  def getHeadersAsEncodedStrings: List[String] =
    headers.map(_.toString())

  def encodeBody(builder: StringBuilder): StringBuilder = {
    builder.append(headers.map {
      header =>
        header.encodeBody()
    }.mkString(Separators.SEMICOLON))
  }

  def encodeAsJValue() = {
    JObject(
      JField("headerNamne", JString(headerName)) ::
        JField("parameters", JArray(headers.map(_.encodeAsJValue()))) :: Nil
    )
  }

  override def encode(builder: StringBuilder) =
    headers match {
      case Nil =>
        builder.append(headerName).append(':').append(Separators.NEWLINE)
      case _ =>
        if (headerName == SIPHeaderNames.WWW_AUTHENTICATE |
          headerName == SIPHeaderNames.PROXY_AUTHENTICATE |
          headerName == SIPHeaderNames.AUTHORIZATION |
          headerName == SIPHeaderNames.PROXY_AUTHORIZATION |
          prettyEncode && headerName == SIPHeaderNames.VIA |
          clazz == classOf[ExtensionHeaderList] |
          headerName == SIPHeaderNames.ROUTE) {
          headers.foreach {
            header =>
              header.encode(builder)
          }
          builder
        } else {
          builder.append(headerName).append(Separators.COLON).append(Separators.SP)
          encodeBody(builder)
          builder.append(Separators.NEWLINE)
        }
    }

  def isEmpty = headers.isEmpty

  def iterator = headers.iterator

  def toList = headers

  protected def createInstance(headers: List[HDR]): A

  def getHead = headers.head

  def getLast = headers.last

  def addHead(sipHeader: HDR): A = createInstance(sipHeader :: headers)

  def addLast(sipHeader: HDR): A = createInstance(headers :+ sipHeader)

  def remove(sipHeader: HDR): A = createInstance(headers.filterNot(_ == sipHeader))

  def removeHead: A = createInstance(headers.drop(1))

  def removeLast: A = createInstance(headers.dropRight(1))

  def concatenate(other: SIPHeaderList[_, _], top: Boolean) = {
    if (top == false) {
      createInstance(headers ++ other.headers.asInstanceOf[List[HDR]])
    } else {
      createInstance(other.headers.asInstanceOf[List[HDR]] ++ headers)
    }
  }

}
