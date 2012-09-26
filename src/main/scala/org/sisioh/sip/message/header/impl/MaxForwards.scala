package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.MaxForwardsHeader
import org.sisioh.sip.util.{SIPDecoder, Decoder, ParserBase, Encoder}

object MaxForwardsDecoder extends MaxForwardsDecoder

class MaxForwardsDecoder extends SIPDecoder[MaxForwards] with MaxForwardsParser {
  def decode(source: String) = decodeTarget(source, Max_ForwardsWithCrLfOpt)
}

trait MaxForwardsParser extends ParserBase {
  lazy val Max_ForwardsWithCrLfOpt = Max_Forwards <~ opt(CRLF)

  lazy val Max_Forwards: Parser[MaxForwards] = "Max-Forwards" ~> HCOLON ~> rep1(DIGIT) ^^ {
    e => MaxForwards(e.mkString.toInt)
  }
}

object MaxForwards {

  def decode(source: String) = MaxForwardsDecoder.decode(source: String)

  object JsonEncoder extends Encoder[MaxForwards] {
    def encode(model: MaxForwards, builder: StringBuilder) = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val json = ("headerName" -> model.headerName) ~
        ("maxForwards" -> model.maxForwards)
      builder.append(compact(render(json)))
    }
  }

}

case class MaxForwards(maxForwards: Int) extends SIPHeader with MaxForwardsHeader {

  val headerName = MaxForwardsHeader.NAME

  def decrementMaxForwards = MaxForwards(maxForwards - 1)

  def encodeByJson(builder: StringBuilder) = encode(builder, MaxForwards.JsonEncoder)

  def encodeBody(builder: StringBuilder) =
    builder.append(maxForwards)

  override def toString = encode()
}
