package org.sisioh.sip.message.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.impl.{StatusLine, ContentLength, RequestLine}
import org.sisioh.sip.message.address.impl.DefaultGenericURI
import org.sisioh.sip.message.{StatusCode, Request}

class SIPResponseDecoderSpec extends Specification with SIPMessageSpecSupport {
  "SIPResponseDecoder" should {
    "StatusLine + ContentLength" in {
      val source = SIPResponseBuilder().
        withStatusLine(Some(StatusLine(StatusCode.OK)))
        .withContentLength(Some(ContentLength(0)))
        .build
      val encode = source.encode()
      println("{" + encode + "}")
      val dest = SIPResponseDecoder.decode(encode)
      dest must_== source
    }
    "StatusLine + ContentLength + To" in {
      val source = SIPResponseBuilder().
        withStatusLine(Some(StatusLine(StatusCode.OK)))
        .withTo(Some(createTo()))
        .withContentLength(Some(ContentLength(0)))
        .build
      val encode = source.encode()
      println("{" + encode + "}")
      val dest = SIPResponseDecoder.decode(encode)
      dest must_== source
    }
  }
}
