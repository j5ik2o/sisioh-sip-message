package org.sisioh.sip.message.header.impl

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

import org.sisioh.sip.message.header.ContentTypeHeader
import org.sisioh.sip.util._
import org.sisioh.sip.core.Separators
import net.liftweb.json.ext.EnumNameSerializer

object ContentTypeDecoder extends ContentTypeDecoder

class ContentTypeDecoder extends SIPDecoder[ContentType] with ContentTypeParser {
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
      val json =
        ("headerName" -> model.headerName) ~
        ("contentType" -> model.contentType) ~
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

  type ParametersHeaderType = ContentType

  val charset = getParameter("charset")

  val mediaRange = MediaRange(contentType, contentSubType)
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
