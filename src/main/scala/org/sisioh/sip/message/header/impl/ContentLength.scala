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

import org.sisioh.sip.message.header.ContentLengthHeader
import org.sisioh.sip.util._
import net.liftweb.json._

object ContentLengthDecoder extends ContentLengthDecoder

class ContentLengthDecoder extends SIPDecoder[ContentLength] with ContentLengthParser {
  def decode(source: String): ContentLength = decodeTarget(source, Content_LengthWithCrLfOpt)
}

trait ContentLengthParser extends ParserBase {
  lazy val Content_LengthWithCrLfOpt = Content_Length <~ opt(CRLF)

  lazy val Content_Length: Parser[ContentLength] = ("Content-Length" | "l") ~> HCOLON ~> rep1(DIGIT) ^^ {
    e => ContentLength(e.mkString.toInt)
  }
}

object ContentLengthEncoder extends SIPEncoder[ContentLength] {

  def encode(model: ContentLength, builder: StringBuilder) =
    builder.append(model.contentLength)

}

trait ContentLengthJsonFieldNames extends JsonFieldNames {
  val CONTENT_LENGTH = "contentLength"
}

object ContentLengthJsonDecoder extends JsonDecoder[ContentLength] with ContentLengthJsonFieldNames {
  def decode(json: JsonAST.JValue) = {
    requireHeaderName(json, ContentLengthHeader.NAME)
    val JInt(contentLength) = json \ CONTENT_LENGTH
    ContentLength(contentLength.toInt)
  }
}

object ContentLengthJsonEncoder extends JsonEncoder[ContentLength] with ContentLengthJsonFieldNames {

  def encode(model: ContentLength) = {
    JObject(
      getHeaderNameAsJValue(model) ::
        JField(CONTENT_LENGTH, JInt(model.contentLength)) :: Nil
    )
  }

}

object ContentLength {

  def decode(source: String) = ContentLengthDecoder.decode(source)

  def decodeFromJson(source: String) = ContentLengthJsonDecoder.decode(source)


}

case class ContentLength(contentLength: Int)
  extends SIPHeader with ContentLengthHeader {

  require(contentLength >= 0)

  val headerName = ContentLengthHeader.NAME
  val name = headerName

  def encodeBody(builder: StringBuilder) = ContentLengthEncoder.encode(this, builder)

  def encodeAsJValue() = ContentLengthJsonEncoder.encode(this)

}
