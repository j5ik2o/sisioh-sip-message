package org.sisioh.sip.message.header.impl

import org.sisioh.sip.core.{GenericObject, Separators}
import org.sisioh.sip.util.Encoder

trait SIPHeader extends GenericObject {
  val headerName: String

  def encodeBody(): String = encodeBody(new StringBuilder()).result()
  def encodeBody(builder: StringBuilder): StringBuilder
  def encodeBody[A](encoder: Encoder[A]): String = encode(new StringBuilder, encoder).result()
  def encodeBody[A](builder: StringBuilder, encoder: Encoder[A]):StringBuilder = encoder.encode(this.asInstanceOf[A], builder)

  def encode(builder: StringBuilder): StringBuilder = {
    builder.append(this.headerName).append(Separators.COLON).append(Separators.SP)
    encodeBody(builder)
    builder.append(Separators.NEWLINE)
    builder
  }
}
