package org.sisioh.sip.message.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.impl.CallId
import org.sisioh.sip.util.Utils

class SIPRequestSpec extends Specification {
  "SIPRequest" should {
    val builder = new SIPRequestBuilder
    val callId = CallId(Utils.generateCallIdentifier("test"))
    val target = builder.withCallId(Some(callId)).build
    target.callId must beSome(callId)
  }
}
