package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.CSeqHeader
import org.sisioh.sip.util.ParserBase

trait CSeqParser extends ParserBase {
  def cseq = "CSeq" ~> ":" ~> rep(DIGIT) ~ Method ^^ {
    case number ~ m =>
      CSeq(m, number.mkString.toLong)
  }
}

case class CSeq(method: String, sequenceNumber: Long) extends CSeqHeader {
  require(sequenceNumber > 0 && sequenceNumber <= (1L << 32 - 1))
  val name = CSeqHeader.NAME
}
