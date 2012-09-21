package org.sisioh.sip.util

/**
 * エンコードできる能力を表すトレイト。
 */
trait Encodable {

  def encode(): String = encode(new StringBuilder).result()
  def encode(builder: StringBuilder): StringBuilder
  def encodeByJson(): String = encodeByJson(new StringBuilder).result()
  def encodeByJson(builder: StringBuilder): StringBuilder

  def encode[A](encoder: Encoder[A]): String = encode(new StringBuilder, encoder).result()
  def encode[A](builder: StringBuilder, encoder: Encoder[A]):StringBuilder = encoder.encode(this.asInstanceOf[A], builder)
}
