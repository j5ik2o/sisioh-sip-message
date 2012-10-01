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

trait MaxForwardsJsonFieldNames extends JsonFieldNames {
  val MAX_FORWARDS = "maxForwards"
}

object MaxForwardsJsonDecoder extends JsonDecoder[MaxForwards] with MaxForwardsJsonFieldNames {

  def decode(json: JsonAST.JValue) = {
    requireHeaderName(json, MaxForwardsHeader.NAME)
    val JInt(maxForwards) = json \ MAX_FORWARDS
    MaxForwards(maxForwards.toInt)
  }

}

object MaxForwardsJsonEncoder extends JsonEncoder[MaxForwards] with MaxForwardsJsonFieldNames {

  def encode(model: MaxForwards) = {
    JObject(
      getHeaderNameAsJValue(model) ::
        JField(MAX_FORWARDS, JInt(model.maxForwards)) :: Nil
    )
  }

}

object MaxForwards {

  def decode(source: String) = MaxForwardsDecoder.decode(source: String)

  def decodeFromJson(source: String) = MaxForwardsJsonDecoder.decode(source)

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
