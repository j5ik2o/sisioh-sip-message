package org.sisioh.sip.message.impl

import org.sisioh.sip.message.address.impl.{DefaultGenericURI, SipUri, DefaultAddressFactory}
import org.sisioh.sip.util._
import org.sisioh.sip.message.header.impl._
import org.sisioh.sip.message.header.Protocol
import org.sisioh.sip.message.Request
import org.sisioh.sip.message.header.impl.ViaList

trait SIPMessageSpecSupport {

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


    val r = SIPRequestBuilder().
      withHeaders(List(via)).
      withTo(Some(createTo())).
      withFrom(Some(createFrom)).
      withCallId(Some(CallId(Utils.generateCallIdentifier("hogehoge")))).
      withCSeq(Some(CSeq(Request.INVITE, 1))).
      withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE)))).
      withMaxForwards(Some(MaxForwards(1))).
      withContentLength(Some(ContentLength(100))).
      withMessageContent(Some(MessageContent("ABC", Some(ContentType("application", "sdp"))))).
      build

    r
  }
}
