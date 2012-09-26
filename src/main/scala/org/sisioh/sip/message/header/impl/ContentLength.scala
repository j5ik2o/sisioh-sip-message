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
import org.sisioh.sip.util.{SIPDecoder, Encoder, Decoder, ParserBase}

object ContentLengthDecoder extends ContentLengthDecoder

class ContentLengthDecoder extends SIPDecoder[ContentLength] with ContentLengthParser {
  def decode(source: String):ContentLength = decodeTarget(source, Content_LengthWithCrLfOpt)
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
      val json = JObject(
        JField("headerName", JString(model.headerName)) ::
        JField("contentLength", JInt(model.contentLength)) :: Nil
      )
      builder.append(compact(render(json)))
    }
  }

}

case class ContentLength(contentLength: Int)
  extends SIPHeader with ContentLengthHeader {

  require(contentLength > 0)
  val headerName = ContentLengthHeader.NAME

  def encodeByJson(builder: StringBuilder) = encode(builder, ContentLength.JsonEncoder)

  def encodeBody(builder: StringBuilder) = {
    if (contentLength < 0)
      builder.append("0")
    else
      builder.append(contentLength)
  }

  override def toString = encode()
}
