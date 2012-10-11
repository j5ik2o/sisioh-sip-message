package org.sisioh.sip.message.header

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

import impl.JsonFieldNames
import org.sisioh.sip.core.{Separators, GenericObject}
import org.sisioh.sip.util.{JsonDecoder, JsonEncoder}
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST.JValue


trait ProtocolJsonFieldNames extends JsonFieldNames {

  protected val PROTOCOL_NAME = "protocolName"

  protected val PROTOCOL_VERSION = "protocolVersion"

  protected val TRANSPORT = "transport"

}

object ProtocolJsonDecoder extends JsonDecoder[Protocol] with ProtocolJsonFieldNames {
  def decode(json: JValue) = {
    val JString(protocolName) = json \ PROTOCOL_NAME
    val JString(protocolVersion) = json \ PROTOCOL_VERSION
    val JString(transport) = json \ TRANSPORT
    Protocol(protocolName, protocolVersion, transport)
  }
}

object ProtocolJsonEncoder extends JsonEncoder[Protocol] with ProtocolJsonFieldNames {

  def encode(model: Protocol) = {
    (PROTOCOL_NAME -> model.protocolName) ~
      (PROTOCOL_VERSION -> model.protocolVersion) ~
      (TRANSPORT -> model.transport)
  }

}

object Protocol {


}

case class Protocol
(protocolName: String = "SIP",
 protocolVersion: String = "2.0",
 transport: String = "UDP")
  extends GenericObject {

  def encode(builder: StringBuilder) = {
    builder.append(protocolName.toUpperCase)
      .append(Separators.SLASH)
      .append(protocolVersion)
      .append(Separators.SLASH)
      .append(transport.toUpperCase)
  }

  def encodeAsJValue() = ProtocolJsonEncoder.encode(this)

}
