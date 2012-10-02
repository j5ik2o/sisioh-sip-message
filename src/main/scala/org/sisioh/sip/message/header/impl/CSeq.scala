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
import org.sisioh.sip.util._
import org.sisioh.sip.core.Separators
import net.liftweb.json._
import org.sisioh.dddbase.core.ValueObjectBuilder

class CSeqBuilder extends ValueObjectBuilder[CSeq, CSeqBuilder]{

  private var method: String = _

  private var sequenceNumber: Long = _

  def withMethod(method: String) = {
    addConfigurator{
      _.method = method
    }
    getThis
  }

  def withSequenceNumber(sequenceNumber: Long) = {
    addConfigurator{
      _.sequenceNumber = sequenceNumber
    }
    getThis
  }

  protected def getThis = this

  protected def newInstance = new CSeqBuilder

  protected def apply(vo: CSeq, builder: CSeqBuilder) {
    builder.withMethod(vo.method)
    builder.withSequenceNumber(vo.sequenceNumber)
  }

  protected def createValueObject = CSeq(method, sequenceNumber)
}

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

trait CSeqJsonFieldNames extends JsonFieldNames {
  val SEQ = "seq"
  val METHOD = "method"
}

object CSeqJsonDecoder extends JsonDecoder[CSeq] with CSeqJsonFieldNames {
  def decode(json: JsonAST.JValue) = {
    requireHeaderName(json, CSeqHeader.NAME)
    val JInt(seq) = json \ SEQ
    val JString(method) = json \ METHOD
    CSeq(method, seq.toLong)
  }
}

object CSeqJsonEncoder extends JsonEncoder[CSeq] with CSeqJsonFieldNames {

  def encode(model: CSeq) = {
    JObject(
      getHeaderNameAsJValue(model) ::
        JField(SEQ, JInt(BigInt(model.sequenceNumber))) ::
        JField(METHOD, JString(model.method)) :: Nil
    )
  }

}

object CSeq {

  def decode(source: String) = CSeqDecoder.decode(source)

  def decodeFromJson(source: String) = CSeqJsonDecoder.decode(source)

}

case class CSeq(method: String, sequenceNumber: Long)
  extends SIPHeader with CSeqHeader {

  require(sequenceNumber > 0 && sequenceNumber <= (1L << 32 - 1))

  val headerName = CSeqHeader.NAME
  val name = headerName

  def encodeBody(builder: StringBuilder) =
    builder.append(sequenceNumber).append(Separators.SP).append(method.toUpperCase)

  def encodeAsJValue() = CSeqJsonEncoder.encode(this)

}
