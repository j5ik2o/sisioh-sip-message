package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class MaxForwardsSpec extends Specification {
  "MaxForwards" should {
    val target = MaxForwards(1)
    target.maxForwards must_== 1
    target.decrementMaxForwards.maxForwards must_== 0
  }
}
