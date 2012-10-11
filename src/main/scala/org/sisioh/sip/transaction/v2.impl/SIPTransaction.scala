package org.sisioh.sip.transaction.impl

import org.sisioh.sip.transport.impl.MessageChannel
import org.sisioh.sip.transaction.{TransactionExt, Transaction}

abstract class SIPTransaction extends MessageChannel with Transaction with TransactionExt {

}
