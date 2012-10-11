package org.sisioh.sip.transaction.v1

import org.sisioh.sip.transport.Peer
import org.sisioh.sip.message.impl.SIPMessage
import org.sisioh.sip.transport.v1.Peer

class InviteClientTransaction
(there: Peer,
 sipMessage: SIPMessage,
 val transactionListener: TransactionListener) extends Transaction {
  val transport = _
}
