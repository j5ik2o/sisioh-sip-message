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

import org.sisioh.sip.message.header.CSeqHeader
import org.sisioh.sip.util.{SIPDecoder, Encoder, Decoder, ParserBase}
import org.sisioh.sip.core.Separators
import net.liftweb.json._

object CSeqDecoder extends CSeqDecoder

class CSeqDecoder extends SIPDecoder[CSeq] with CSeqParser {
  def decode(source: String) = decodeTarget(source, cseqWithCrLfOpt)
}

trait CSeqParser extends ParserBase {

  lazy val cseqWithCrLfOpt: Parser[CSeq] = cseq <~ opt(CRLF)

  lazy val cseq: Parser[CSeq] = "CSeq" ~> (HCOLON ~> (rep1(DIGIT) ~ (LWS ~> Method))) ^^ {
    case number ~ m =>
      CSeq(m, number.mkString.toLong)
  }

}

object CSeq {

  def decode(source: String) = CSeqDecoder.decode(source)

  def decodeFromJson(source: String) = JsonDecoder.decode(source)

  object JsonDecoder extends Decoder[CSeq] {
    def decode(source: String) = {
      import net.liftweb.json._
      val json = parse(source)
      val JInt(seq) = json \ "seq"
      val JString(method) = json \ "method"
      CSeq(method, seq.toLong)
    }
  }

  object JsonEncoder extends Encoder[CSeq] {
    def encode(model: CSeq, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(
        JField("headerName", JString(model.headerName)) ::
          JField("seq", JInt(BigInt(model.sequenceNumber))) ::
          JField("method", JString(model.method)) :: Nil
      )
      builder.append(compact(render(json)))
    }
  }

}

case class CSeq(method: String, sequenceNumber: Long) extends SIPHeader with CSeqHeader {
  require(sequenceNumber > 0 && sequenceNumber <= (1L << 32 - 1))

  val headerName = CSeqHeader.NAME

  def encodeByJson(builder: StringBuilder) = encode(builder, CSeq.JsonEncoder)

  def encodeBody(builder: StringBuilder) =
    builder.append(sequenceNumber).append(Separators.SP).append(method.toUpperCase)

  override def toString = encode()
}
