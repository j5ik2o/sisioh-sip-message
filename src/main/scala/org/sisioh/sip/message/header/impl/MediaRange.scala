package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util._
import org.sisioh.sip.core.Separators
import scala.Some

object MediaRangeDecoder extends MediaRangeDecoder

class MediaRangeDecoder extends SIPDecoder[MediaRange] with MediaRangeParser {
  def decode(source: String) = decodeTarget(source, mediaRange)
}

trait MediaRangeParser extends ParserBase {
  lazy val mediaRange = mediaType ^^ {
    case mt =>
      MediaRange(mt._1, mt._2)
  }

  lazy val mediaType: Parser[(String, String, NameValuePairList)] = mType ~ (SLASH ~> mSubtype) ~ rep(SEMI ~> mParameter) ^^ {
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
  lazy val mParameter: Parser[NameValuePair] = mAttribute ~ (EQUAL ~> mValue) ^^ {
    case ma ~ mv => NameValuePair(Some(ma), Some(mv))
  }
  lazy val mAttribute = token
  lazy val mValue = token | quotedString
}

object MediaRange {

  def decode(source: String) = MediaRangeDecoder.decode(source)

  object JsonEncoder extends Encoder[MediaRange] {
    def encode(model: MediaRange, builder: StringBuilder) = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val json = ("type" -> model.mediaType) ~
        ("subType" -> model.mediaSubType)
      builder.append(compact(render(json)))
    }
  }

}

case class MediaRange(mediaType: String, mediaSubType: String) extends SIPObject {

  def encode(builder: StringBuilder) = {
    builder.append(mediaType)
      .append(Separators.SLASH)
      .append(mediaSubType)
    builder
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, MediaRange.JsonEncoder)

  override def toString = encode()
}
