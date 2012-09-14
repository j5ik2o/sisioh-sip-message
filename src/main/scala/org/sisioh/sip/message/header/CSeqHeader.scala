package org.sisioh.sip.message.header

trait CSeqHeader extends Header {

  val method: String
  val sequenceNumber: Int

}

object CSeqHeader {
  val NAME = "CSeq"
}
