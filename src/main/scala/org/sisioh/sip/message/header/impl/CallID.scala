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

class CallIDDecoder extends SIPDecoder[CallId] with CallIDParser {

  def decode(source: String): CallId = decodeTarget(source, Call_IDWithCrLfOpt)

}

trait CallIDParser extends ParserBase {

  lazy val Call_IDWithCrLfOpt: Parser[CallId] = Call_ID <~ opt(CRLF)

  lazy val Call_ID: Parser[CallId] = ("Call-ID" | "i") ~> (HCOLON ~> callid) ^^ {
    case callid =>
      CallId(callid)
  }

  lazy val callid: Parser[String] = repsep(word, "@") ^^ {
    _.mkString("@")
  }

}

object CallId {

  def decode(source: String): CallId = CallIDDecoder.decode(source)

  def decodeFromJson(source: String): CallId = JsonDecoder.decode(source)

  object JsonDecoder extends Decoder[CallId] {
    def decode(source: String) = {
      import net.liftweb.json._
      val json = parse(source)
      val JString(callId) = json \ "callId"
      CallId(callId)
    }
  }

  object JsonEncoder extends Encoder[CallId] {
    def encode(model: CallId, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(
        JField("headerName", JString(model.headerName)) ::
        JField("callId", JString(model.callId)) :: Nil
      )
      builder.append(compact(render(json)))
    }
  }

}

case class CallId(callId: String) extends SIPHeader with CallIdHeader {
  val callIdentity = CallIdentifier.from(callId)
  val headerName = CallIdHeader.NAME

  def encodeByJson(builder: StringBuilder) = encode(builder, CallId.JsonEncoder)

  def encodeBody(builder: StringBuilder) =
    callIdentity.encode(builder)

  override def toString = encode()
}
