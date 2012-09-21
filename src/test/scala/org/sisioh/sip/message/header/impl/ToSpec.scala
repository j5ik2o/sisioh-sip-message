package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.address.impl.{SipUri, DefaultAddressFactory}
import org.sisioh.sip.core.Separators

class ToSpec extends Specification {

  "To" should {
    val address = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
    val to = To(address)
    println(to.encode())
    to.encode() must_== """To: "kato" <sip:hoge@localhost>""" + Separators.NEWLINE
    to.address must_== address
  }

}
