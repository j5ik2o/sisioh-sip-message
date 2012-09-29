package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class CSeqDecoderSpec extends Specification {
  "CSeqDecoder" should {
    "標準エンコード" in {
      val source = CSeq("INVITE", 1)
      val encode = source.encode()
      println(encode)
      val dest = CSeqDecoder.decode(encode)
      "可逆的にデコードできること" in {
        source must_== dest
      }
    }
    "JSONエンコード" in {
      val source = CSeq("INVITE", 1)
      val encode = source.encodeByJson()
      println(encode)
      val dest = CSeqJsonDecoder.decode(encode)
      "可逆的にデコードできること" in {
        source must_== dest
      }
    }
  }
}
