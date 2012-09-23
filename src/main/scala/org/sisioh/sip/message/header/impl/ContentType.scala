package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ContentTypeHeader
import org.sisioh.sip.util.{Decoder, NameValuePair, NameValuePairList, ParserBase}

object ContentTypeDecoder {
  def apply() = new ContentTypeDecoder
}

class ContentTypeDecoder extends Decoder with ContentTypeParser {
  def decode(source: String) = decodeTarget(source, contentType)
}

trait ContentTypeParser extends ParserBase {
  lazy val contentType: Parser[ContentType] = ("Content-Type" | "c") ~> ":" ~> mediaType ^^ {
    case mt =>
      ContentType(mt._1, Some(mt._2), mt._3)
  }

  lazy val mediaType: Parser[(String, String, NameValuePairList)] = mType ~ ("/" ~> mSubtype) ~ rep(";" ~> mParameter) ^^ {
    case mt ~ msub ~ mparams =>
      (mt, msub, mparams.foldLeft(NameValuePairList())((l, r) => l.add(r)))

  }

  lazy val mType = discreteType | compositeType

  lazy val discreteType = "text" | "image" | "audio" | "video" | "application" | extensionToken
  lazy val compositeType = "message" | "multipart" | extensionToken
  lazy val extensionToken = ietfToken | xToken
  lazy val ietfToken = token
  lazy val xToken: Parser[String] = "x-" ~ token ^^ {
    case f ~ s => f + s
  }
  lazy val mSubtype = extensionToken | ianaToken
  lazy val ianaToken = token
  lazy val mParameter: Parser[NameValuePair] = mAttribute ~ ("=" ~> mValue) ^^ {
    case ma ~ mv => NameValuePair(Some(ma), Some(mv))
  }
  lazy val mAttribute = token
  lazy val mValue = token | quotedString
}

case class ContentType
(contentType: String,
 contentSubType: Option[String],
 parameters: NameValuePairList) extends ContentTypeHeader {

  val name = ContentTypeHeader.NAME

  def parameterNames = parameters.names

  def getParameter(name: String) = parameters.getValue(name)

  def removeParameter(name: String) = {
    new ContentType(
      contentType,
      contentSubType,
      parameters.remove(name)
    )
  }
}
