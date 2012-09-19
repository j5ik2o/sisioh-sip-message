package org.sisioh.sip.util


trait Encodable {

  def encode(): String = encode(new StringBuilder).result()
  def encode(builder: StringBuilder): StringBuilder
  def encode[A](encoder: Encoder[A]): String = encode(new StringBuilder, encoder).result()
  def encode[A](builder: StringBuilder, encoder: Encoder[A]):StringBuilder = encoder.encode(this.asInstanceOf[A], builder)
}
