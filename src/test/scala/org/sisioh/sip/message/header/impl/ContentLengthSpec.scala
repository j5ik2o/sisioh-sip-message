package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.core.Separators

class ContentLengthSpec extends Specification {
  "ContentLength" should {
    val target = ContentLength(100)
    "属性が取得できること" in {
      target.contentLength must_== 100
    }
    "正しいエンコード結果が得られること" in {
      target.encode() must_== """Content-Length: 100""" + Separators.NEWLINE
      target.encodeByJson() must_== """{"headerName":"Content-Length","contentLength":100}"""
    }
  }
}
