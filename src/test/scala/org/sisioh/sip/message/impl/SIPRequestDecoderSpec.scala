package org.sisioh.sip.message.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.impl._
import org.sisioh.sip.message.Request
import org.sisioh.sip.message.address.impl.DefaultGenericURI
import scala.Some
import org.sisioh.sip.core.Separators

class SIPRequestDecoderSpec extends Specification with SIPMessageSpecSupport {

  "SIPRequestDecoder" should {
    "RequestLine + ContentLength" in {
      val source = SIPRequestBuilder().
        withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE))))
        .withContentLength(Some(ContentLength(0)))
        .build
      val encode = source.encode()
      val dest = SIPRequestDecoder.decode(encode)
      dest must_== source
    }
    "RequestLine + ContentLength + To" in {
      val source = SIPRequestBuilder().
        withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE))))
        .withTo(Some(createTo()))
        .withContentLength(Some(ContentLength(0)))
        .build
      val encode = source.encode()
      val dest = SIPRequestDecoder.decode(encode)
      dest must_== source
    }
  }

}
