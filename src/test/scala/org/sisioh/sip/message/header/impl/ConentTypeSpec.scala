package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.NameValuePairList
import org.sisioh.sip.core.Separators

class ConentTypeSpec extends Specification {
  "ConentType" should {
    val target = ContentType("application", "sdp")
    "正しい属性が取得できる" in {
      target.contentType must_== "application"
      target.contentSubType must_== "sdp"
      target.parameters must_== NameValuePairList()
    }
    "エンコード結果が取得できる" in {
      target.encode() must_== "Content-Type: application/sdp" + Separators.NEWLINE
      target.encodeByJson() must_== """{"headerName":"Content-Type","contentType":"application","contentSubType":"sdp","parameters":{}}"""
    }
  }
}
