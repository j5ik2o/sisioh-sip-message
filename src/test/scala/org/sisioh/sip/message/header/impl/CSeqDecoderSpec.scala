package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class CSeqDecoderSpec extends Specification {
  "CSeqDecoder" should {
    val source = CSeq("INVITE",1)
    val encode = source.encode()
    println(encode)
    val dest = CSeqDecoder.decode(encode)
    "可逆的にデコードできること" in {
      source must_== dest
    }
  }
}
