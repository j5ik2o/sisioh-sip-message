package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.CallIdHeader
import org.sisioh.sip.util.{Decoder, Encoder, ParserBase}

object CallIDDecoder extends CallIDDecoder

class CallIDDecoder extends Decoder with CallIDParser {
  def decode(source: String) = decodeTarget(source, Call_IDWithCrLfOpt)
}

trait CallIDParser extends ParserBase {

  lazy val Call_IDWithCrLfOpt: Parser[CallID] = Call_ID <~ opt(CRLF)

  lazy val Call_ID: Parser[CallID] = ("Call-ID" | "i") ~> (HCOLON ~> callid) ^^ {
    case callid =>
      CallID(callid)
  }

  lazy val callid: Parser[String] = repsep(word, "@") ^^ {
    _.mkString("@")
  }

}

object CallID {

  def decode(source: String) = CallIDDecoder.decode(source)

  object JsonEncoder extends Encoder[CallID] {
    def encode(model: CallID, builder: StringBuilder) = {
      import net.liftweb.json._
//      val json = JObject(JField("address", parse(model.address.encodeByJson())) ::
//        JField("paramters", parse(model.parameters.encodeByJson())) :: Nil)
//      builder.append(compact(render(json)))
      null
    }
  }

}

case class CallID(callId: String) extends SIPHeader with CallIdHeader {
  val name = CallIdHeader.NAME
  val callIdentity = CallIdentifier(callId)
  val headerName = CallIdHeader.NAME

  def encodeByJson(builder: StringBuilder) = encode(builder, CallID.JsonEncoder)

  def encodeBody(builder: StringBuilder) =
    callIdentity.encode(builder)

}
