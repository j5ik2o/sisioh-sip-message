package org.sisioh.sip.transport

import org.specs2.mutable.Specification
import org.sisioh.sip.message.impl._
import scala.concurrent.ops._
import org.sisioh.sip.message.header.impl.{Via, ContentLength, RequestLine}
import org.sisioh.sip.message.address.impl.DefaultGenericURI
import org.sisioh.sip.message.Request
import org.sisioh.sip.util.{Host, HostPort}
import org.sisioh.sip.message.header.Protocol
import scala.Some
import org.sisioh.sip.message.header.impl.ViaList

class TCPServerTransportSpec extends Specification with TransportListener with SIPMessageSpecSupport {
  var sipMessage: SIPMessage = _
  //  def main(args: Array[String]) {
  "test" in {
    import org.sisioh.sip.util.Loan._
    using(TCPServerTransport(2002, Some(this))) {
      target =>
        spawn {
          target.accept()
        }
        Thread.sleep(5000)

        using(TCPTransport.connect(Peer("localhost", 2002), Some(this))) {
          client =>
            val via1 = Via(HostPort(Host("localhost"), None), Protocol("SIP", "2.0", "TCP"))
            val via2 = Via(HostPort(Host("localhost"), None), Protocol("SIP", "2.0", "UDP"))
            val via = ViaList(List(via1, via2))

            val request = SIPRequestBuilder().
              withHeaders(List(via)).
              withTo(Some(createTo())).
              withRequestLine(Some(RequestLine(DefaultGenericURI("test:test"), Some(Request.INVITE)))).
              withContentLength(Some(ContentLength("abc".getBytes().length))).
              withMessageContent(Some(MessageContent("abc"))).
              build

            client.sendMessage(request)
            Thread.sleep(500)
            sipMessage must_== request
        }.get
    }.get
  }


  def onReceivedFromTransport(transport: Transport, sipMessage_ : SIPMessage) {
    printf("onReceivedFromTransport(%s,%s)\n", transport, sipMessage_)
    sipMessage = sipMessage_
  }

  def onConnected(transport: Transport) {
    printf("onConnected(%s)\n", transport)
  }

  def onDisconnected(transport: Transport) {
    printf("onDisconnected(%s)\n", transport)
  }

}
