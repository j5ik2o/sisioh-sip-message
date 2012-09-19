package org.sisioh.sip.message.header

trait CSeqHeader extends Header {

  val method: String
  val sequenceNumber: Long

}

object CSeqHeader {
  val NAME = "CSeq"
}
