package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.address.impl.{SipUri, DefaultAddressFactory}
import org.sisioh.sip.core.Separators
import org.sisioh.sip.util.{NameValuePair, NameValuePairList}

class ToSpec extends Specification {

  "To" should {
    "アドレスのみ指定した場合" in {
      val address = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
      val to = To(address)
      to.encode() must_== """To: "kato" <sip:hoge@localhost>""" + Separators.NEWLINE
      to.encodeByJson() must_== """{"address":{"uri":{"scheme":"sip","authority":{"hostPort":{"host":"localhost"},"userInfo":{"name":"hoge"}},"uriParams":{},"qheaders":{}},"displayName":"kato","addressType":0},"paramters":{}}"""
      to.address must_== address
    }
    "アドレスとパラメータを指定した場合" in {
      val address = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
      val params = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
      val to = To(address, None, params)
      to.encode() must_== """To: "kato" <sip:hoge@localhost>;a=b""" + Separators.NEWLINE
      to.encodeByJson() must_== """{"address":{"uri":{"scheme":"sip","authority":{"hostPort":{"host":"localhost"},"userInfo":{"name":"hoge"}},"uriParams":{},"qheaders":{}},"displayName":"kato","addressType":0},"paramters":{"a":"b"}}"""
      to.address must_== address
      to.parameters must_== params
    }
  }

}
