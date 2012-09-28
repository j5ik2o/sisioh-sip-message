package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.address.impl.DefaultGenericURI

class RequestLineDecoderSpec extends Specification {

  "RequestLineDecoder" should {
    "可逆的にデコードできること" in {
      "標準" in {
        val source = RequestLine(DefaultGenericURI("test:testurl"), Some("INVITE"))
        val encode = source.encode()
        val dest = RequestLineDecoder.decode(encode)
        dest must_== source
      }
      "JSON" in {
        val source = RequestLine(DefaultGenericURI("test:testurl"), Some("INVITE"))
        val encode = source.encodeByJson()
        val dest = RequestLine.decodeFromJson(encode)
        dest must_== source
      }
    }
  }

}
