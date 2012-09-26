package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.{SIPHeaderNames, Header}
import org.sisioh.sip.core.Separators
import collection.immutable.List
import collection.{LinearSeq, mutable, LinearSeqOptimized, immutable}
import collection.mutable.{Builder, ListBuffer}
import collection.generic.GenericTraversableTemplate

abstract class SIPHeaderList[A <: SIPHeaderList[A, HDR], HDR <: SIPHeader]
(val clazz: Class[_],
 val headerName: String,
 val prettyEncode: Boolean = false,
 private val headers: List[HDR] = List.empty[HDR])
  extends SIPHeader with Header {

  def encodeBody(builder: StringBuilder): StringBuilder = {
    builder.append(headers.map {
      header =>
        header.encodeBody()
    }.mkString(Separators.SEMICOLON))
  }

  def encodeByJson(builder: StringBuilder) = {
    import net.liftweb.json._
    val json = JObject(
      JField("headerNamne", JString(headerName)) ::
      JField("parameters", JArray(headers.map(e => parse(e.encodeByJson())))) :: Nil)
    builder.append(compact(render(json)))
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

  def toList = headers

  protected def createInstance(headers: List[HDR]): A

  def getHead = headers.head

  def getLast = headers.last

  def addHead(sipHeader: HDR): A = createInstance(sipHeader :: headers)

  def addLast(sipHeader: HDR): A = createInstance(headers :+ sipHeader)

  def remove(sipHeader: HDR): A = createInstance(headers.filterNot(_ == sipHeader))

  def concatenate(other: SIPHeaderList[A, HDR], top: Boolean) = {
    if (top == false) {
      createInstance(headers ++ other.headers)
    } else {
      createInstance(other.headers ++ headers)
    }
  }

}
