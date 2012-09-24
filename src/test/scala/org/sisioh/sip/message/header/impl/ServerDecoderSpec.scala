package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class ServerDecoderSpec extends Specification {
  "ServerDecoder" should {
    val source = Server(List(Product("PRODUCT")))
    val encode = source.encode()
    val dest = ServerDecoder.decode(encode)
    "可逆的にデコードできること" in {
      dest must_== source
    }
  }
}
