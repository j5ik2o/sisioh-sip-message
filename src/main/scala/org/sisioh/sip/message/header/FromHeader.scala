package org.sisioh.sip.message.header

trait FromHeader extends HeaderAddress with Parameters with Header {

  val tag: String

}

object FromHeader {

  val NAME = "From"

}
