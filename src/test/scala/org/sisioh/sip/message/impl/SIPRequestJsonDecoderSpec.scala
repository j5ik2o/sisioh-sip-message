package org.sisioh.sip.message.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.impl.{ContentLength, RequestLine}
import org.sisioh.sip.message.address.impl.DefaultGenericURI
import org.sisioh.sip.message.Request
import net.liftweb.json._

class SIPRequestJsonDecoderSpec extends Specification with SIPMessageSpecSupport{
  "SIPRequestJsonDecoder" should {
    "可逆的にデコードできること" in {
      val source = SIPRequestBuilder().
        withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE))))
        .withContentLength(Some(ContentLength("abc".getBytes.size)))
        .withMessageContent(Some(MessageContent("abc")))
        .build
      val encode = source.encodeByJson()
      println(encode)
      parseOpt(encode) must beSome
      val dest = SIPRequestJsonDecoder.decode(encode)
      println("dest = "+dest)
      dest must_== source
    }
  }
}
