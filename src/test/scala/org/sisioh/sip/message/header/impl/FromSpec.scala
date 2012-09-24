package org.sisioh.sip.message.header.impl

/*
 * Copyright 2012 Sisioh Project and others. (http://www.sisioh.org/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */

import org.specs2.mutable.Specification
import org.sisioh.sip.message.address.impl.{SipUri, DefaultAddressFactory}
import org.sisioh.sip.core.Separators
import org.sisioh.sip.util.{NameValuePair, NameValuePairList}

class FromSpec extends Specification {

  "From" should {
    "アドレスのみ指定した場合" in {
      val address = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
      val from = From(address)
      from.encode() must_== """From: "kato" <sip:hoge@localhost>""" + Separators.NEWLINE
      from.encodeByJson() must_== """{"address":{"uri":{"scheme":"sip","authority":{"hostPort":{"host":"localhost"},"userInfo":{"name":"hoge"}},"uriParams":{},"qheaders":{}},"displayName":"kato","addressType":0},"paramters":{}}"""
      from.address must_== address
    }
    "アドレスとパラメータを指定した場合" in {
      val address = new DefaultAddressFactory().createAddress(SipUri.decode("sip:hoge@localhost"), Some("kato"))
      val params = NameValuePairList.fromValues(List(NameValuePair(Some("a"), Some("b"))))
      val from = From(address, None, params)
      from.encode() must_== """From: "kato" <sip:hoge@localhost>;a=b""" + Separators.NEWLINE
      from.encodeByJson() must_== """{"address":{"uri":{"scheme":"sip","authority":{"hostPort":{"host":"localhost"},"userInfo":{"name":"hoge"}},"uriParams":{},"qheaders":{}},"displayName":"kato","addressType":0},"paramters":{"a":"b"}}"""
      from.address must_== address
      from.parameters must_== params
    }
  }

}
