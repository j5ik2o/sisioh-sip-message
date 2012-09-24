package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class CallIDDecoderSpec extends Specification {
  "CallIDDecoder" should {
    val source = CallID("test@test")
    val encode = source.encode()
    val dest = CallIDDecoder.decode(encode)
    "可逆的にデコードできること" in {
    source must_== dest
    }
  }
}
