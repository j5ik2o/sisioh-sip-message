package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class CallIdDecoderSpec extends Specification {
  "CallIdDecoder" should {
    "標準エンコード" in {
      val source = CallId("test@test")
      val encode = source.encode()
      val dest = CallIdDecoder.decode(encode)
      "可逆的にデコードできること" in {
        source must_== dest
      }
    }
    "JSONエンコード" in {
      val source = CallId("test@test")
      val encode = source.encodeByJson()
      val dest = CallId.decodeFromJson(encode)
      "可逆的にデコードできること" in {
        source must_== dest
      }
    }
  }
}
