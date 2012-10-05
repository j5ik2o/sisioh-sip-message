package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.StatusCode

class StatusLineDecoderSpec extends Specification {
  "StatusLineDecoder" should {
    "可逆的にデコードできること" in {
      "標準" in {
        val source = StatusLine(StatusCode(200))
        val encode = source.encode()
        println(encode)
        val dest = StatusLineDecoder.decode(encode)
        dest must_== source
      }
      "JSON" in {
        val source = StatusLine(StatusCode(200))
        val encode = source.encodeByJson()
        println(encode)
        val dest = StatusLineJsonDecoder.decode(encode)
        dest must_== source
      }
    }
  }
}
