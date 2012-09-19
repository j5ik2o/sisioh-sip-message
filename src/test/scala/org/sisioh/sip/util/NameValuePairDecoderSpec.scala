package org.sisioh.sip.util

import org.specs2.mutable.Specification
import org.sisioh.sip.core.Separators

class NameValuePairDecoderSpec extends Specification {

  "NameValuePairDecoder" should {
    "エンコードしたモデルをデコードできる" in {
      "名前と値がある場合" in {
        val source = NameValuePair(Some("name"), Some("value"), Separators.AT, "'")
        val encode = source.encode()
        println(encode)
        val target = NameValuePairDecoder(Separators.AT, "'")
        val dest = target.decode(encode)
        dest must_== source
      }
      "値がなし,isQuotedString=trueの場合" in {
        val source = NameValuePair(Some("name"), Some(""), Separators.AT, "'", true)
        val encode = source.encode()
        println(encode)
        val target = NameValuePairDecoder(Separators.AT, "'", true)
        val dest = target.decode(encode)
        dest must_== source
      }
      "値がなし,isQuotedString=falseの場合" in {
        val source = NameValuePair(Some("name"), Some(""), Separators.AT, "'", false)
        val encode = source.encode()
        println(encode)
        val target = NameValuePairDecoder(Separators.AT, "'", true)
        val dest = target.decode(encode)
        dest must_== source
      }
    }
  }

}
