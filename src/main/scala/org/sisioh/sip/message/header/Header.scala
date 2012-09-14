package org.sisioh.sip.message.header

@cloneable
trait Header extends Serializable {
  val name: String
}