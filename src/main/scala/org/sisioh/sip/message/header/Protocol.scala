package org.sisioh.sip.message.header

import org.sisioh.sip.core.{Separators, GenericObject}

object Protocol{

}

case class Protocol(protocolName: String, protocolVersion: String, transport: String) extends GenericObject {

  def encode(builder: StringBuilder) = {
    builder.append(protocolName.toUpperCase)
      .append(Separators.SLASH)
      .append(protocolVersion)
      .append(Separators.SLASH)
      .append(transport.toUpperCase)
  }

  def encodeByJson(builder: StringBuilder) = null

}
