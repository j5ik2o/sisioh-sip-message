package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.CSeqHeader
import org.sisioh.sip.util.{Encoder, Decoder, ParserBase}
import org.sisioh.sip.core.Separators
import net.liftweb.json._

object CSeqDecoder extends CSeqDecoder

class CSeqDecoder extends Decoder with CSeqParser {
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

  object JsonEncoder extends Encoder[CSeq] {
    def encode(model: CSeq, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(
        JField("seq", JInt(BigInt(model.sequenceNumber))) ::
          JField("method", JString(model.method)) :: Nil
      )
      builder.append(compact(render(json)))
    }
  }

}

case class CSeq(method: String, sequenceNumber: Long) extends SIPHeader with CSeqHeader {
  require(sequenceNumber > 0 && sequenceNumber <= (1L << 32 - 1))

  val name = CSeqHeader.NAME
  val headerName = CSeqHeader.NAME

  def encodeByJson(builder: StringBuilder) = encode(builder, CSeq.JsonEncoder)

  def encodeBody(builder: StringBuilder) =
    builder.append(sequenceNumber).append(Separators.SP).append(method.toUpperCase)

  override def toString = encode()
}
