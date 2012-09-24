package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class ContentLengthDecoderSpec extends Specification {
  "ContentLengthDecoder" should {
    val source = ContentLength(100)
    val encode = source.encode()
    println(encode)
    val dest = ContentLengthDecoder.decode(encode)
    "可逆的にデコードできること" in {
      source must_== dest
    }
  }
}
