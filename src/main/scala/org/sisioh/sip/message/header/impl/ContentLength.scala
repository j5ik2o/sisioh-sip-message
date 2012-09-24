package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ContentLengthHeader
import org.sisioh.sip.util.{Encoder, Decoder, ParserBase}

object ContentLengthDecoder extends ContentLengthDecoder

class ContentLengthDecoder extends Decoder with ContentLengthParser {
  def decode(source: String) = decodeTarget(source, Content_LengthWithCrLfOpt)
}

trait ContentLengthParser extends ParserBase {
  lazy val Content_LengthWithCrLfOpt =  Content_Length <~ opt(CRLF)

  lazy val Content_Length: Parser[ContentLength] = ("Content-Length" | "l") ~> HCOLON ~> rep1(DIGIT) ^^ {
    e => ContentLength(e.mkString.toInt)
  }
}

object ContentLength {

  def decode(source: String) = ContentLengthDecoder.decode(source)

  object JsonEncoder extends Encoder[ContentLength]{
    def encode(model: ContentLength, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("contentLength", JInt(model.contentLength)) :: Nil)
      builder.append(compact(render(json)))
    }
  }

}

case class ContentLength(contentLength: Int)
  extends SIPHeader with ContentLengthHeader {

  require(contentLength > 0)
  val name = ContentLengthHeader.NAME
  val headerName = ContentLengthHeader.NAME

  def encodeByJson(builder: StringBuilder) = encode(builder, ContentType.JsonEncoder)

  def encodeBody(builder: StringBuilder) = {
    if (contentLength < 0)
      builder.append("0")
    else
      builder.append(contentLength)
  }

  override def toString = encode()
}
