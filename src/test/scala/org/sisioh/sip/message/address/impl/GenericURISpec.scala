package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification

class GenericURISpec extends Specification {
  "GenericURI" should {
    val scheme = "test"
    val uriString = scheme + ":abc"
    val target = DefaultGenericURI(uriString)
    "SipURIではないこと" in {
      target.isSipURI must_== false
    }
    "スキームが取得できること" in {
      target.scheme must_== scheme
    }
    "URI文字列が取得できること" in {
      target.uriString must_== uriString
    }
    "エンコード結果が正しいこと" in {
      target.encode() must_== uriString
    }
  }
}
