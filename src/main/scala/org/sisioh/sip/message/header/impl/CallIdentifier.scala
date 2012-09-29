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

import org.sisioh.sip.util.{JsonDecoder, JsonEncoder, Encoder, Encodable}
import org.sisioh.sip.core.{GenericObject, Separators}
import net.liftweb.json._

object CallIdentifierEncoder extends Encoder[CallIdentifier] {

  def encode(model: CallIdentifier, builder: StringBuilder) = {
    model.host.map {
      builder.append(model.localId).append(Separators.AT).append(_)
    }.getOrElse {
      builder.append(model.localId)
    }
  }

}


object CallIdentifierJsonDecoder extends JsonDecoder[CallIdentifier] {

  def decode(json: JsonAST.JValue) = {
    val JString(localId) = json \ "localId"
    val hostOpt = (json \ "host").toOpt.map {
      _.asInstanceOf[JString].s
    }
    CallIdentifier(localId, hostOpt)
  }

}

object CallIdentifierJsonEncoder extends JsonEncoder[CallIdentifier] {

  def encode(model: CallIdentifier) = {
    JObject(model.host.map {
      h =>
        JField("localId", JString(model.localId)) :: JField("host", JString(h)) :: Nil
    }.getOrElse(JField("localId", JString(model.localId)) :: Nil))
  }

}

object CallIdentifier {

  def from(localIdWithHost: String): CallIdentifier = {
    val index = localIdWithHost.indexOf('@')
    if (index == -1) {
      CallIdentifier(localIdWithHost, None)
    } else {
      val localId = localIdWithHost.substring(0, index)
      val host = localIdWithHost.substring(index + 1, localIdWithHost.length())
      CallIdentifier(localId, Some(host))
    }
  }

}

case class CallIdentifier(localId: String, host: Option[String] = None) extends GenericObject {

  def encode(builder: StringBuilder) = CallIdentifierEncoder.encode(this, builder)

  def encodeAsJValue() = CallIdentifierJsonEncoder.encode(this)

  override def toString = encode()

}
