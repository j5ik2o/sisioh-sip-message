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

import org.sisioh.sip.core.{Separators, GenericObject}
import org.sisioh.sip.util.JsonEncoder
import net.liftweb.json.JsonDSL._

object ProtocolJsonEncoder extends JsonEncoder[Protocol] {

  def encode(model: Protocol) = {
    ("protocolName" -> model.protocolName) ~
      ("protocolVersion" -> model.protocolVersion) ~
      ("transport" -> model.transport)
  }

}

object Protocol {


}

case class Protocol
(protocolName: String,
 protocolVersion: String,
 transport: String)
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
