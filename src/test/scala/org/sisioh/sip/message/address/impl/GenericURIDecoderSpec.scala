package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification

class GenericURIDecoderSpec extends Specification {

  "GenericURIDecoder" should {
    val uri = DefaultGenericURI("test:abc")
    val target = DefaultGenericURIDecoder()
    val encodeObject = uri.encode()
    val result = target.decode(encodeObject)
    "可逆的にデコードできること" in {
      result must_== uri
    }
  }

}
