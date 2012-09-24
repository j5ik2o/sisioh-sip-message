package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class ContentTypeDecoderSpec extends Specification {
  "ContentTypeDecoder" should {
    val source = ContentType("application", "sdp")
    val encode = source.encode()
    val dest = ContentTypeDecoder.decode(encode)
    "可逆的にデコードできること" in {
      source must_== dest
    }
  }
}
