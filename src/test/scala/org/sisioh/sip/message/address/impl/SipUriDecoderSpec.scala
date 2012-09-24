package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification

class SipUriDecoderSpec extends Specification {

  "SipUriDecoder" should {
    val uri = SipUri.fromUserAndHost(Some("user"), None, Some("localhost"), None)
    val encodeObject = uri.encode()
    val result = SipUriDecoder.decode(encodeObject)
    "可逆的にデコードできること" in {
      result must_== uri
    }
  }

}
