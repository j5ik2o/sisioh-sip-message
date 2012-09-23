package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.{HostDecoder, Host}
import org.sisioh.sip.message.address.impl.{SipUri, DefaultAddress}
import org.sisioh.sip.message.address.URI

class ToDecoderSpec extends Specification {


  "ToDecoder" should {
    val decoder = ToDecoder()
    "displayName" in {
      //      "empty" in {
      //        val displayName = ""
      //        decoder.decodeTarget(displayName, decoder.displayName) must_== displayName
      //      }
      "quoteless" in {
        val displayName = "ABC DEF "
        decoder.decodeTarget(displayName, decoder.displayName) must_== displayName
      }
      "quoted" in {
        val displayName = "\"aaaa\""
        decoder.decodeTarget(displayName, decoder.displayName) must_== displayName.filterNot(_ == '"')
      }
    }
    "nameAddr" in {
      "withDisplayName" in {
        val nameAddr = "\"aaaa\" <sip:kato@localhost>"
        val r = decoder.decodeTarget(nameAddr, decoder.nameAddr)
        r._1 must beSome("aaaa")
        r._2 must_== SipUri.decode("sip:kato@localhost")
      }
      "withoutDisplayName" in {
        val nameAddr = "<sip:kato@localhost>"
        val r = decoder.decodeTarget(nameAddr, decoder.nameAddr)
        r._1 must beNone
        r._2 must_== SipUri.decode("sip:kato@localhost")
      }
    }
    "nameAddrToDefaultAddress" in {
      val nameAddr = "\"aaaa\" <sip:kato@localhost>"
      val r = decoder.decodeTarget(nameAddr, decoder.nameAddrToDefaultAddress)
      r must_== DefaultAddress(SipUri.decode("sip:kato@localhost"), Some("aaaa"))
    }
    "addrSpecToDefaultAddress" in {
      val addrSpec = "sip:kato@localhost"
      val r = decoder.decodeTarget(addrSpec, decoder.addrSpecToDefaultAddress)
      r must_== DefaultAddress(SipUri.decode("sip:kato@localhost"))
    }
    "To" in {
      val dest = ToDecoder().decode("To: \"kato\" <sip:kato@localhost>")
      dest must_== To(DefaultAddress(SipUri.fromUserAndHost(Some("kato"), None, Some("localhost"), None), Some("kato")))
    }
    "可逆的にデコードできること" in {
      val source = To(DefaultAddress(SipUri.fromUserAndHost(Some("test"), None, Some("localhost"), None), Some("kato")))
      val encodeObj = source.encode()
      val dest = ToDecoder().decode(encodeObj)
      source must_== dest
    }
  }

}
