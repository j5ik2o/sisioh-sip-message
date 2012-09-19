package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.CSeqHeader

case class CSeq(method: String, sequenceNumber: Long) extends CSeqHeader {
  require(sequenceNumber > 0 && sequenceNumber <= (1L << 32 - 1))
  val name = CSeqHeader.NAME
}
