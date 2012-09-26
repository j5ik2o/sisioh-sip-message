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

import org.sisioh.sip.message.header.CallIdHeader
import org.sisioh.sip.util.{SIPDecoder, Decoder, Encoder, ParserBase}

object CallIDDecoder extends CallIDDecoder

class CallIDDecoder extends SIPDecoder[CallID] with CallIDParser {

  def decode(source: String): CallID = decodeTarget(source, Call_IDWithCrLfOpt)

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

  def decode(source: String): CallID = CallIDDecoder.decode(source)

  def decodeFromJson(source: String): CallID = JsonDecoder.decode(source)

  object JsonDecoder extends Decoder[CallID] {
    def decode(source: String) = {
      import net.liftweb.json._
      val json = parse(source)
      val JString(callId) = json \ "callId"
      CallID(callId)
    }
  }

  object JsonEncoder extends Encoder[CallID] {
    def encode(model: CallID, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(
        JField("headerName", JString(model.headerName)) ::
        JField("callId", JString(model.callId)) :: Nil
      )
      builder.append(compact(render(json)))
    }
  }

}

case class CallID(callId: String) extends SIPHeader with CallIdHeader {
  val callIdentity = CallIdentifier.from(callId)
  val headerName = CallIdHeader.NAME

  def encodeByJson(builder: StringBuilder) = encode(builder, CallID.JsonEncoder)

  def encodeBody(builder: StringBuilder) =
    callIdentity.encode(builder)

  override def toString = encode()
}
