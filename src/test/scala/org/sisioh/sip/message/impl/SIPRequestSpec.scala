package org.sisioh.sip.message.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.impl._
import org.sisioh.sip.util.{NameValuePair, NameValuePairList, Utils}
import org.sisioh.sip.message.address.impl.{SipUri, DefaultAddressFactory, DefaultGenericURI}
import org.sisioh.sip.core.Separators
import scala.Some
import org.sisioh.sip.message.Request

class SIPRequestSpec extends Specification {
  "SIPRequest" should {
    "callIdが取得できること" in {
      val builder = SIPRequestBuilder()
      val callId = CallId(Utils.generateCallIdentifier("test"))
      val target = builder.withCallId(Some(callId)).build
      target.callId must beSome(callId)
    }
    "エンコード結果が取得できること" in {
      "RequestLineのメソッド名が指定されてなければCSeqのメソッド名を利用できること" in {
        val builder = SIPRequestBuilder()
        val target2 = builder.
          withCSeq(Some(CSeq("INVITE", 1))).
          withRequestLine(Some(RequestLine(DefaultGenericURI("test:test")))).build
        val encodeObject = target2.encode()
        val lines = encodeObject.split(Separators.NEWLINE)
        lines(0) must_== """INVITE test:test SIP/2.0"""
        lines(1) must_== """CSeq: 1 INVITE"""
      }
      "すべてのヘッダーを変換できること" in {

        val toAddress = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
        val toParams = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
        val to = To(toAddress, None, toParams)

        val fromAddress = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
        val fromParams = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
        val from = From(fromAddress, None, fromParams)

        val target2 = SIPRequestBuilder().
          withTo(Some(to)).
          withFrom(Some(from)).
          withCallId(Some(CallId(Utils.generateCallIdentifier("hogehoge")))).
          withCSeq(Some(CSeq(Request.INVITE, 1))).
          withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE)))).
          withMaxForwards(Some(MaxForwards(1))).
          withContentLength(Some(ContentLength(100))).
          withMessageContent(
          Some(MessageContent(ContentType("application", "sdp"), "ABC"))
        ).build

        val encodeRequest = target2.encode()
        println(encodeRequest)
        val lines = encodeRequest.split(Separators.NEWLINE)

        lines(0) must_== "INVITE test:test SIP/2.0"
        lines(1) must_== """To: "kato" <sip:hoge@localhost>;a=b"""
        lines(2) must_== """From: "kato" <sip:hoge@localhost>;a=b"""
        lines(3) must beMatching( """Call-ID: [a-zA-Z0-9]+@hogehoge""".r)
        lines(4) must_== """CSeq: 1 INVITE"""
        lines(5) must_== """Max-Forwards: 1"""
        lines(6) must_== """Content-Type: application/sdp"""
        lines(7) must_== """Content-Length: 100"""
      }
    }
  }
}
