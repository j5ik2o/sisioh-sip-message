package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.core.Separators

class CallIdSpec extends Specification {
  "CallId" should {
    val callId = "test@testhost"
    "callIdが正しいこと" in {
      CallId(callId).callId must_== "test@testhost"
    }
    "CallIdentityが正しいこと" in {
      CallId(callId).callIdentity.localId must_== "test"
      CallId(callId).callIdentity.host must beSome("testhost")
    }
    "エンコード結果が正しいこと" in {
      CallId(callId).encode() must_== "Call-ID: test@testhost" + Separators.NEWLINE
      CallId(callId).encodeByJson() must_== """{"headerName":"Call-ID","callId":"test@testhost"}"""
    }
  }
}
