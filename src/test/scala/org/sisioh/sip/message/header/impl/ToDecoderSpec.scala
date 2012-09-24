package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.address.impl.{SipUri, DefaultAddress}

class ToDecoderSpec extends Specification {

  "ToDecoder" should {
    "displayName" in {
      "quoteless" in {
        val displayName = "ABC DEF "
        ToDecoder.decodeTarget(displayName, ToDecoder.displayName) must_== displayName
      }
      "quoted" in {
        val displayName = "\"aaaa\""
        ToDecoder.decodeTarget(displayName, ToDecoder.displayName) must_== displayName.filterNot(_ == '"')
      }
    }
    "nameAddr" in {
      "withDisplayName" in {
        val nameAddr = "\"aaaa\" <sip:kato@localhost>"
        val r = ToDecoder.decodeTarget(nameAddr, ToDecoder.nameAddr)
        r._1 must beSome("aaaa")
        r._2 must_== SipUri.decode("sip:kato@localhost")
      }
      "withoutDisplayName" in {
        val nameAddr = "<sip:kato@localhost>"
        val r = ToDecoder.decodeTarget(nameAddr, ToDecoder.nameAddr)
        r._1 must beNone
        r._2 must_== SipUri.decode("sip:kato@localhost")
      }
    }
    "nameAddrToDefaultAddress" in {
      val nameAddr = "\"aaaa\" <sip:kato@localhost>"
      val r = ToDecoder.decodeTarget(nameAddr, ToDecoder.nameAddrToDefaultAddress)
      r must_== DefaultAddress(SipUri.decode("sip:kato@localhost"), Some("aaaa"))
    }
    "addrSpecToDefaultAddress" in {
      val addrSpec = "sip:kato@localhost"
      val r = ToDecoder.decodeTarget(addrSpec, ToDecoder.addrSpecToDefaultAddress)
      r must_== DefaultAddress(SipUri.decode("sip:kato@localhost"))
    }
    "To" in {
      val dest = ToDecoder.decode("To: \"kato\" <sip:kato@localhost>")
      dest must_== To(DefaultAddress(SipUri.fromUserAndHost(Some("kato"), None, Some("localhost"), None), Some("kato")))
    }
    "可逆的にデコードできること" in {
      val source = To(DefaultAddress(SipUri.fromUserAndHost(Some("test"), None, Some("localhost"), None), Some("kato")))
      val encodeObj = source.encode()
      val dest = ToDecoder.decode(encodeObj)
      source must_== dest
    }
  }

}
