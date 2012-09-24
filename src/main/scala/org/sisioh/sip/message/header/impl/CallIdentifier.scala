package org.sisioh.sip.message.header.impl

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
}
