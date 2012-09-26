package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.core.Separators

class MaxForwardsSpec extends Specification {
  "MaxForwards" should {
    val target = MaxForwards(1)
    "正しい属性が取得できること" in {
      target.maxForwards must_== 1
    }
    "デクリメントできること" in {
      target.decrementMaxForwards.maxForwards must_== 0
    }
    "エンコード結果を取得できること" in {
      target.encode() must_== """Max-Forwards: 1""" + Separators.NEWLINE
      target.encodeByJson() must_== """{"headerName":"Max-Forwards","maxForwards":1}"""
    }
  }
}
