package org.sisioh.sip.message.header.impl

import org.sisioh.sip.core.Separators

trait SIPHeader {
  val headerName: String

  def encodeBody(): String = encodeBody(new StringBuilder()).result()

  def encodeBody(builder: StringBuilder): StringBuilder

  def encode(): String = {
    encode(new StringBuilder()).result
  }

  def encode(builder: StringBuilder): StringBuilder = {
    builder.append(this.headerName).append(Separators.COLON).append(Separators.SP);
    encodeBody(builder)
    builder.append(Separators.NEWLINE)
    builder
  }
}
