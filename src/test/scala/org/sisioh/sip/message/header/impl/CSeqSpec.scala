package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class CSeqSpec extends Specification {
  "CSeq" should {
    "正しい属性が取得できる" in {
      CSeq("INVITE", 1).method must_== "INVITE"
      CSeq("INVITE", 1).sequenceNumber must_== 1
    }
  }
}
