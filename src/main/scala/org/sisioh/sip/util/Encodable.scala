package org.sisioh.sip.util


trait Encodable[A] {
  this : A =>

  def encode(): String = encode(new StringBuilder).result()
  def encode(builder: StringBuilder): StringBuilder
  def encode(encoder: Encoder[A]): String = encode(new StringBuilder, encoder).result()
  def encode(builder: StringBuilder, encoder: Encoder[A]):StringBuilder = encoder.encode(this, builder)
}
