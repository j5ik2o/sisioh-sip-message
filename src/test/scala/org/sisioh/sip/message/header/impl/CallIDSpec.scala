package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.core.Separators

class CallIDSpec extends Specification {
  "CallID" should {
    val callId = "test@testhost"
    "callIdが正しいこと" in {
      CallID(callId).callId must_== "test@testhost"
    }
    "CallIdentityが正しいこと" in {
      CallID(callId).callIdentity.localId must_== "test"
      CallID(callId).callIdentity.host must beSome("testhost")
    }
    "エンコード結果が正しいこと" in {
      CallID(callId).encode() must_== "Call-ID: test@testhost" + Separators.NEWLINE
    }
  }
}
