package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification

class DefaultAddressJsonDecoderSpec extends Specification {

  "DefaultAddressJsonDecoder" should {
    "JSON" in {
      val uri = SipUri.fromUserAndHost(Some("user"), None, "localhost", None)
      "表示名がない場合" in {
        val source = DefaultAddress(uri)
        val encodeObject = source.encodeByJson()
        println(encodeObject)
        val dest = DefaultAddressJsonDecoder.decode(encodeObject)
        println(dest.encodeByJson())
        dest must_== source
      }
      "表示名がある場合" in {
        val source = DefaultAddress(uri, Some("kato"))
        val encodeObject = source.encodeByJson()
        val dest = DefaultAddressJsonDecoder.decode(encodeObject)
        dest must_== source
      }
      "ワイルドカードの場合" in {
        val source = DefaultAddress(WildCardURI)
        val encodeObject = source.encodeByJson()
        val dest = DefaultAddressJsonDecoder.decode(encodeObject)
        dest must_== source
      }
    }
  }

}
