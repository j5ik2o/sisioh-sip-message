package org.sisioh.sip.util

trait Encodable[A] {
  this : A =>

  def encode(implicit encoder: Encoder[A]): String = encode(new StringBuilder).result()
  def encode(builder: StringBuilder)(implicit encoder: Encoder[A]): StringBuilder = encoder.encode(this, builder)

}
