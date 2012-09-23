package org.sisioh.sip.util

import org.specs2.mutable.Specification
import org.sisioh.sip.core.Separators

class NameValuePairListDecoderSpec extends Specification {
  "NameValuePairListDecoder" should {
    "エンコードしたモデルをデコードできる" in {
      val nv = NameValuePair(Some("name"), Some("value"))
      val source = NameValuePairList.fromValues(List(nv))
      val encode = source.encode()
      println(encode)
      val target = NameValuePairListDecoder()
      val dest = target.decode(encode)
      dest must_== source
    }
  }
}
