package org.sisioh.sip.message.header

trait FromHeader extends HeaderAddress with Parameters with Header {

  val tag: Option[String]

}

object FromHeader {

  val NAME = "From"

}
