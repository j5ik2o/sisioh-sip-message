package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ContentTypeHeader
import org.sisioh.sip.util._
import org.sisioh.sip.core.Separators
import scala.Some
import net.liftweb.json.ext.EnumNameSerializer

object ContentTypeDecoder extends ContentTypeDecoder

class ContentTypeDecoder extends Decoder with ContentTypeParser {
  def decode(source: String) = decodeTarget(source, contentTypeWithCrLfOpt)
}

trait ContentTypeParser extends ParserBase with MediaRangeParser {
  lazy val contentTypeWithCrLfOpt = contentType <~ opt(CRLF)

  lazy val contentType: Parser[ContentType] = ("Content-Type" | "c") ~> HCOLON ~> mediaType ^^ {
    case mt =>
      ContentType(mt._1, mt._2, mt._3)
  }

}

object ContentType {

  def decode(source: String) = ContentTypeDecoder.decode(source)

  object JsonEncoder extends Encoder[ContentType] {
    def encode(model: ContentType, builder: StringBuilder) = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      implicit val formats = net.liftweb.json.DefaultFormats + new EnumNameSerializer(AddressType)
      val json = ("contentType" -> model.contentType) ~
        ("contentSubType" -> model.contentSubType) ~
        ("parameters" -> parse(model.parameters.encodeByJson()))
      builder.append(compact(render(json)))
    }
  }

}

case class ContentType
(contentType: String,
 contentSubType: String,
 parameters: NameValuePairList = NameValuePairList()) extends ParametersHeader with ContentTypeHeader {
  val mediaRange = MediaRange(contentType, contentSubType)
  val name = ContentTypeHeader.NAME
  val headerName = ContentTypeHeader.NAME
  val duplicates = DuplicateNameValueList()

  def encodeByJson(builder: StringBuilder) = encode(builder, ContentType.JsonEncoder)

  def encodeBody(builder: StringBuilder) = {
    mediaRange.encode(builder)
    if (hasParameters) {
      builder.append(Separators.SEMICOLON)
      parameters.encode(builder)
    }
    builder
  }

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new ContentType(contentType, contentSubType, _parameters)
  }

  override def toString = encode()

}
