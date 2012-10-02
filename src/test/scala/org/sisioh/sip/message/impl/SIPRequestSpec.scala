package org.sisioh.sip.message.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.impl._
import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl.{SipUri, DefaultAddressFactory, DefaultGenericURI}
import org.sisioh.sip.core.Separators
import scala.Some
import org.sisioh.sip.message.Request
import scala.Some
import org.sisioh.sip.message.header.Protocol
import scala.Some
import org.sisioh.sip.message.header.impl.ViaList

class SIPRequestSpec extends Specification {

  def createTo(uri: String = "sip:hoge@localhost", displayName: String = "kato") = {
    val toAddress = new DefaultAddressFactory().createAddress(SipUri.decode(uri), Some(displayName))
    val toParams = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
    To(toAddress, None, toParams)
  }

  def createFrom = {
    val fromAddress = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
    val fromParams = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
    From(fromAddress, None, fromParams)
  }

  def createBasicRequest = {

    val via1 = Via(HostPort(Host("localhost"), None), Protocol("SIP", "2.0", "TCP"))
    val via2 = Via(HostPort(Host("localhost"), None), Protocol("SIP", "2.0", "TCP"))
    val via = ViaList(List(via1, via2))


    SIPRequestBuilder().
      withHeaders(List(via)).
      withTo(Some(createTo())).
      withFrom(Some(createFrom)).
      withCallId(Some(CallId(Utils.generateCallIdentifier("hogehoge")))).
      withCSeq(Some(CSeq(Request.INVITE, 1))).
      withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE)))).
      withMaxForwards(Some(MaxForwards(1))).
      withContentLength(Some(ContentLength(100))).
      withMessageContent(Some(MessageContent(ContentType("application", "sdp"), "ABC"))).
      build
  }

  "SIPRequest" should {
    "トランザクションIDを取得できること" in {
      createBasicRequest.getTransactionId must beMatching( """[a-zA-Z0-9]+-hogehoge-1-invite-localhost""" +Utils.signature )
    }
    "ダイアログIDが取得できること" in {
      "CallIDのみの場合" in {
        val builder = SIPRequestBuilder()
        val callId = CallId(Utils.generateCallIdentifier("test"))
        val target = builder.withCallId(Some(callId)).build
        val dialogId = target.getDialogId(true, None)
        dialogId.get must beMatching( """[a-zA-Z0-9]+@test""".r)
      }
      "CallIDおよびFromタグがある場合" in {
        val builder = SIPRequestBuilder()
        val fromAddress = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
        val fromParams = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
        val from = From(fromAddress, Some("hoge"), fromParams)
        val callId = CallId(Utils.generateCallIdentifier("test"))
        val target = builder.withFrom(Some(from)).withCallId(Some(callId)).build
        val dialogId = target.getDialogId(true, None)
        println(dialogId)
        dialogId.get must beMatching( """[a-zA-Z0-9]+@test:hoge""".r)
      }
      "CallIDおよびToタグがある場合" in {
        val builder = SIPRequestBuilder()
        val toAddress = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
        val toParams = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
        val to = To(toAddress, Some("fuga"), toParams)
        val callId = CallId(Utils.generateCallIdentifier("test"))
        val target = builder.withTo(Some(to)).withCallId(Some(callId)).build
        val dialogId = target.getDialogId(true)
        println(dialogId)
        dialogId.get must beMatching( """[a-zA-Z0-9]+@test:fuga""".r)
      }
      "CallIDおよびTo,Fromタグがある場合" in {
        val builder = SIPRequestBuilder()
        val fromAddress = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
        val fromParams = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
        val from = From(fromAddress, Some("hoge"), fromParams)
        val toAddress = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
        val toParams = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
        val to = To(toAddress, Some("fuga"), toParams)
        val callId = CallId(Utils.generateCallIdentifier("test"))
        val target = builder.withFrom(Some(from)).withTo(Some(to)).withCallId(Some(callId)).build
        val dialogId = target.getDialogId(true)
        println(dialogId)
        dialogId.get must beMatching( """[a-zA-Z0-9]+@test:fuga:hoge""".r)
        "isServerがfalseの時" in {
          val dialogId = target.getDialogId(false)
          dialogId.get must beMatching( """[a-zA-Z0-9]+@test:hoge:fuga""".r)
        }
      }
    }
    "callIdが取得できること" in {
      val builder = SIPRequestBuilder()
      val callId = CallId(Utils.generateCallIdentifier("test"))
      val target = builder.withCallId(Some(callId)).build
      target.callId must beSome(callId)
    }
    "既存のリクエストからACKリクエストを生成する" in {
      val to = createTo("sip:test@localhost", "TEST")
      val request = createBasicRequest
      val ackRequest = request.createAckRequest(to)
      val encodeObject = ackRequest.encode()
      println(ackRequest.encode())
      ackRequest.to must beSome(to)
      val lines = encodeObject.split(Separators.NEWLINE)
      lines(0) must_== """ACK test:test SIP/2.0"""
      lines(1) must_== """Via: SIP/2.0/TCP localhost"""
      lines(2) must_== """To: "TEST" <sip:test@localhost>;a=b"""
      lines(3) must_== """From: "kato" <sip:hoge@localhost>;a=b"""
      lines(4) must beMatching( """Call-ID: [a-zA-Z0-9]+@hogehoge""".r)
      lines(5) must_== """CSeq: 1 ACK"""
      lines(6) must_== """Max-Forwards: 1"""
      lines(7) must_== """Content-Length: 0"""
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
      "headersにContentLengthを追加した場合" in {
        val contentLength = ContentLength(200)
        val builder = SIPRequestBuilder()
        val target2 = builder.
          withHeaders(List(contentLength)).build
        val encodeObject = target2.encode()
        val lines = encodeObject.split(Separators.NEWLINE)
        lines(0) must_== """Content-Length: 200"""
      }
      "すべてのヘッダーを変換できること" in {
        val target = createBasicRequest
        target.vaildateHeaders

        val encodeRequest = target.encode()
        println(encodeRequest)
        val lines = encodeRequest.split(Separators.NEWLINE)

        lines(0) must_== "INVITE test:test SIP/2.0"
        lines(1) must_== """Via: SIP/2.0/TCP localhost;SIP/2.0/TCP localhost"""
        lines(2) must_== """To: "kato" <sip:hoge@localhost>;a=b"""
        lines(3) must_== """From: "kato" <sip:hoge@localhost>;a=b"""
        lines(4) must beMatching( """Call-ID: [a-zA-Z0-9]+@hogehoge""".r)
        lines(5) must_== """CSeq: 1 INVITE"""
        lines(6) must_== """Max-Forwards: 1"""
        lines(7) must_== """Content-Type: application/sdp"""
        lines(8) must_== """Content-Length: 100"""
      }
    }
  }
}
