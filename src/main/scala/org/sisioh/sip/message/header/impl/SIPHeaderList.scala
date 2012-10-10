package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.{SIPHeaderNames, Header}
import org.sisioh.sip.core.Separators
import collection.immutable.List
import net.liftweb.json._
import org.sisioh.sip.util.{JsonDecoder, JsonEncoder}

trait SIPHeaderListJsonFieldNames extends JsonFieldNames

abstract class SIPHeaderListJsonDecoder[A <: SIPHeaderList[A, HDR], HDR <: SIPHeader]
  extends JsonDecoder[A] with SIPHeaderListJsonFieldNames {

  protected def createInstance(sipHeaders: List[HDR]): A

  def decode(json: JsonAST.JValue) = {
    val JArray(parameters) = json \ PARAMETERS
    val headers = parameters.map {
      e =>
        ViaJsonDecoder.decode(e)
    }
    createInstance(headers.map(_.asInstanceOf[HDR]))
  }

}

class SIPHeaderListJsonEncoder[A <: SIPHeaderList[A, HDR], HDR <: SIPHeader]
  extends JsonEncoder[A] with SIPHeaderListJsonFieldNames {

  def encode(model: A) = {
    JObject(
      JField(HEADER_NAME, JString(model.headerName)) ::
        JField(PARAMETERS, JArray(model.headers.map(_.encodeAsJValue()))) :: Nil
    )
  }

}


abstract class SIPHeaderList[A <: SIPHeaderList[A, HDR], HDR <: SIPHeader]
(val clazz: Class[_],
 val headerName: String,
 val prettyEncode: Boolean = false,
 private[impl] val headers: List[HDR] = List.empty[HDR])
  extends SIPHeader with Header {

  override def toString = encode()

  protected val jsonEncoder = new SIPHeaderListJsonEncoder[A, HDR]

  def getHeadersAsEncodedStrings: List[String] =
    headers.map(_.toString())

  def encodeBody(builder: StringBuilder): StringBuilder =
    encodeBody(builder, Separators.SEMICOLON)

  def encodeBody(builder: StringBuilder, separator: String) = {
    builder.append(headers.map {
      header =>
        header.encodeBody()
    }.mkString(separator))
  }

  def encodeAsJValue() = jsonEncoder.encode(this.asInstanceOf[A])

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

  def removeAll: A = createInstance(List.empty)

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
