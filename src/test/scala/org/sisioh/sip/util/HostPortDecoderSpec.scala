package org.sisioh.sip.util

import org.specs2.mutable.Specification

class HostPortDecoderSpec extends Specification {

  "HostPortDecoder" should {
    val host = HostPort(Host("www.dwango.co.jp"), None)
    val encode = host.encode()
    println("(%s)".format(encode))
    val h = HostPortDecoder().decode(encode)
    "可逆的にデコードできること" in {
      host must_== h
    }
  }

}
