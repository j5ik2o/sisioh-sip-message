package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification

class DefaultAddressDecoderSpec extends Specification {

  "DefaultAddressDecoder" should {
    "可逆的にデコードできること" in {
      "標準" in {
        val uri = SipUri.fromUserAndHost(Some("user"), None, "localhost", None)
        "表示名がない場合" in {
          val source = DefaultAddress(uri)
          val encodeObject = source.encode()
          val dest = DefaultAddressDecoder.decode(encodeObject)
          dest must_== source
        }
        "表示名がある場合" in {
          val source = DefaultAddress(uri, Some("kato"))
          val encodeObject = source.encode()
          val dest = DefaultAddressDecoder.decode(encodeObject)
          dest must_== source
        }
        "ワイルドカードの場合" in {
          val source = DefaultAddress(WildCardURI)
          val encodeObject = source.encode()
          val dest = DefaultAddressDecoder.decode(encodeObject)
          dest must_== source
        }
      }

    }
  }

}
