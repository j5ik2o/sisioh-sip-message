package org.sisioh.sip.message.header

trait MaxForwardsHeader {
  val maxForwards: Int
  def decrementMaxForwards: MaxForwardsHeader
}

object MaxForwardsHeader {
  val NAME = "Max-Forwards"
}
