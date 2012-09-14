package org.sisioh.sip.util

/**
 * モデルを符号化するエンコーダ。
 *
 * @tparam A モデルの型
 */
trait Encoder[A] {
  def encode(model: A, builder: StringBuilder): StringBuilder
}
