package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.core.Separators

class CSeqSpec extends Specification {
  "CSeq" should {
    "正しい属性が取得できる" in {
      CSeq("INVITE", 1).method must_== "INVITE"
      CSeq("INVITE", 1).sequenceNumber must_== 1
    }
    "正しいエンコード結果が取得できること" in {
      CSeq("INVITE", 1).encode() must_== """CSeq: 1 INVITE""" + Separators.NEWLINE
      CSeq("INVITE", 1).encodeByJson() must_== """{"headerName":"CSeq","seq":1,"method":"INVITE"}"""
    }
  }
}
