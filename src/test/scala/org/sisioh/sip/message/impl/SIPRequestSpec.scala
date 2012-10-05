package org.sisioh.sip.message.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.impl._
import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl.{SipUri, DefaultAddressFactory, DefaultGenericURI}
import org.sisioh.sip.core.Separators
import scala.Some
import org.sisioh.sip.message.Request
import scala.Some
import org.sisioh.sip.message.header.{SIPConstants, Protocol}
import scala.Some
import org.sisioh.sip.message.header.impl.ViaList

class SIPRequestSpec extends Specification with SIPMessageSpecSupport {

  "SIPRequest" should {
    "トランザクションIDを取得できること" in {
      createBasicRequest.getTransactionId must beMatching( """[a-zA-Z0-9]+-hogehoge-1-invite-localhost""" + Utils.signature)
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
      ackRequest.to must beSome(to)
      ackRequest.headers must contain(to)
      ackRequest.headers must contain(request.from.get)
      ackRequest.headers must contain(request.maxForwards.get)
      ackRequest.headers must contain(request.callId.get)
      ackRequest.headers must contain(request.contentLength.get)
      ackRequest.requestLine.get.method must beSome(Request.ACK)
      ackRequest.requestLine.get.uri.uriString must_== "test:test"
      ackRequest.requestLine.get.sipVersion must beSome(SIPConstants.SIP_VERSION_STRING)
      ackRequest.getViaHeadHeader.get.protocol must_== "SIP"
      ackRequest.getViaHeadHeader.get.protocolVersion must_== "2.0"
      ackRequest.getViaHeadHeader.get.transport must_== "TCP"
      ackRequest.getViaHeadHeader.get.host must_== "localhost"
      ackRequest.getViaHeadHeader.get.port must beNone
      ackRequest.viaHost must beSome("localhost")
      ackRequest.viaPort must beNone
      ackRequest.cSeq.get.method must_== Request.ACK
      ackRequest.cSeq.get.sequenceNumber must_== 1L
    }
    "エンコード結果が取得できること" in {
      "RequestLineのメソッド名が指定されてなければCSeqのメソッド名を利用できること" in {
        val builder = SIPRequestBuilder()
        val target = builder.
          withCSeq(Some(CSeq(Request.INVITE, 1))).
          withRequestLine(Some(RequestLine(DefaultGenericURI("test:test")))).build
        target.requestLine.get.method must beNone
        target.requestLine.get.uri.uriString must_== "test:test"
        target.requestLine.get.sipVersion must beSome(SIPConstants.SIP_VERSION_STRING)
        target.cSeq.get.method must_== Request.INVITE
        target.cSeq.get.sequenceNumber must_== 1L
      }
      "headersにContentLengthを追加した場合" in {
        val contentLength = ContentLength(200)
        val builder = SIPRequestBuilder()
        val target = builder.
          withHeaders(List(contentLength)).build
        target.contentLength must beSome(contentLength)
      }
      "すべてのヘッダーを変換できること" in {
        val target = createBasicRequest
        target.validateHeaders

        target.requestLine.get.method must beSome(Request.INVITE)
        target.requestLine.get.uri.uriString must_== "test:test"
        target.requestLine.get.sipVersion must beSome(SIPConstants.SIP_VERSION_STRING)
        target.getViaHeadHeader.get.protocol must_== "SIP"
        target.getViaHeadHeader.get.protocolVersion must_== "2.0"
        target.getViaHeadHeader.get.transport must_== "TCP"
        target.getViaHeadHeader.get.host must_== "localhost"
        target.getViaHeadHeader.get.port must beNone
        target.headers must contain(target.to.get)
        target.headers must contain(target.from.get)
        target.headers must contain(target.maxForwards.get)
        target.headers must contain(target.callId.get)
        target.headers must contain(target.cSeq.get)
        target.headers must contain(target.maxForwards.get)
        target.headers must contain(target.contentType.get)
        target.headers must contain(target.contentLength.get)
      }
    }
  }
}
