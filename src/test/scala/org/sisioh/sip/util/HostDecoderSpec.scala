package org.sisioh.sip.util

import org.specs2.mutable.Specification
import org.sisioh.sip.message.address.impl.{UserInfoDecoder, UserInfo}

class HostDecoderSpec extends Specification {

  "HostDecoder" should {
    val host = Host("localhost")
    val encode = host.encode()
    val h = HostDecoder.decode(encode)
    "可逆的にデコードできること" in {
      host must_== h
    }
  }

}
