package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util.Encodable
import org.sisioh.sip.core.{GenericObject, Separators}

object CallIdentifier {

  def apply(localIdWithHost: String): CallIdentifier = {
    val index = localIdWithHost.indexOf('@')
    if (index == -1) {
      CallIdentifier(localIdWithHost)
    } else {
      val localId = localIdWithHost.substring(0, index)
      val host = localIdWithHost.substring(index + 1, localIdWithHost.length())
      CallIdentifier(localId, Some(host))
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

}
