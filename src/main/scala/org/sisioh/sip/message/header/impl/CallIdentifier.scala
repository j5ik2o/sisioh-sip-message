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

import org.sisioh.sip.util.{Encoder, Encodable}
import org.sisioh.sip.core.{GenericObject, Separators}
import annotation.tailrec

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

  object JsonEncoder extends Encoder[CallIdentifier] {
    def encode(model: CallIdentifier, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(model.host.map{h =>
        JField("localId", JString(model.localId)) :: JField("host", JString(h)) :: Nil
      }.getOrElse(JField("localId", JString(model.localId)) :: Nil))
      builder.append(compact(render(json)))
    }

  }

}

case class CallIdentifier(localId: String, host: Option[String] = None) extends GenericObject {

  def encode(builder: StringBuilder) = {
    host.map {
      builder.append(localId).append(Separators.AT).append(_)
    }.getOrElse {
      builder.append(localId)
    }
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, CallIdentifier.JsonEncoder)

  override def toString = encode()
}
