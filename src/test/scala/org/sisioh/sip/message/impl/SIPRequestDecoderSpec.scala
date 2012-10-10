package org.sisioh.sip.message.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.impl._
import org.sisioh.sip.message.Request
import org.sisioh.sip.message.address.impl.DefaultGenericURI
import scala.Some
import org.sisioh.sip.core.Separators
import org.sisioh.sip.util.{Host, HostPort}
import org.sisioh.sip.message.header.Protocol

class SIPRequestDecoderSpec extends Specification with SIPMessageSpecSupport {

  "SIPRequestDecoder" should {
    "Via" in {

      val via1 = Via(HostPort(Host("localhost"), None), Protocol("SIP", "2.0", "TCP"))
      val via2 = Via(HostPort(Host("localhost"), None), Protocol("SIP", "2.0", "UDP"))
      val via = ViaList(List(via1, via2))
      val source = SIPRequestBuilder().
        withHeaders(List(via)).
        withTo(Some(createTo())).
        withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE)))).
        withContentLength(Some(ContentLength("abc".getBytes().length))).
        withMessageContent(Some(MessageContent("abc"))).
        build
      val encode = source.encode()
      println(encode)
      val dest = SIPRequestDecoder.decode(encode)
      dest must_== source

    }

    "RequestLine + ContentLength == 0" in {
      val source = SIPRequestBuilder().
        withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE))))
        .withContentLength(Some(ContentLength(0)))
        .build
      val encode = source.encode()
      println("{" + encode + "}")
      val dest = SIPRequestDecoder.decode(encode)
      dest must_== source
    }
    "RequestLine + ContentLength == 0 + To" in {
      val source = SIPRequestBuilder().
        withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE))))
        .withTo(Some(createTo()))
        .withContentLength(Some(ContentLength(0)))
        .build
      val encode = source.encode()
      println("{" + encode + "}")
      val dest = SIPRequestDecoder.decode(encode)
      dest must_== source
    }
    "RequestLine + ContentLength + Content == abc" in {
      val source = SIPRequestBuilder().
        withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE))))
        .withContentLength(Some(ContentLength("abc".getBytes().length)))
        .withMessageContent(Some(MessageContent("abc")))
        .build
      val encode = source.encode()
      val dest = SIPRequestDecoder.decode(encode)
      dest must_== source
    }
  }

}
