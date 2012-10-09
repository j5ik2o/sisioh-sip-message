package org.sisioh.sip.message.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.impl.{ContentLength, RequestLine}
import org.sisioh.sip.message.address.impl.DefaultGenericURI
import org.sisioh.sip.message.Request

class SIPMessageDecoderSpec extends Specification with SIPMessageSpecSupport {

  "SIPMessageDecoder" should {
    "可逆的メッセージをデコードできる" in {
      val source = SIPRequestBuilder().
        withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE))))
        .withContentLength(Some(ContentLength("abc".getBytes().length)))
        .withMessageContent(Some(MessageContent("abc")))
        .build
      val encode = source.encode()
      val dest = SIPMessageDecoder.decode(encode)
      dest must_== source
    }
  }

}
