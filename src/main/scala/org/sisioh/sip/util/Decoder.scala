package org.sisioh.sip.util

trait Decoder[A] {

  def decode(source: String): A

}
