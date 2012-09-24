package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class MaxForwardsDecoderSpec extends Specification {
  "MaxForwardsDecoder" should {
    val source = MaxForwards(1)
    val encode = source.encode()
    val dest = MaxForwardsDecoder.decode(encode)
    "可逆的にデコードできること" in {
      dest must_== source
    }
  }
}
