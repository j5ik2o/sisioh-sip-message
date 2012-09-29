package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.MaxForwardsHeader
import org.sisioh.sip.util._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._


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

object MaxForwardsJsonEncoder extends JsonEncoder[MaxForwards] {

  def encode(model: MaxForwards) = {
    ("headerName" -> model.headerName) ~
      ("maxForwards" -> model.maxForwards)
  }

}

object MaxForwards {

  def decode(source: String) = MaxForwardsDecoder.decode(source: String)


}

case class MaxForwards(maxForwards: Int) extends SIPHeader with MaxForwardsHeader {

  val headerName = MaxForwardsHeader.NAME

  val name = headerName

  def decrementMaxForwards = MaxForwards(maxForwards - 1)

  def encodeBody(builder: StringBuilder) =
    builder.append(maxForwards)

  def encodeAsJValue() = MaxForwardsJsonEncoder.encode(this)

  override def toString = encode()
}
