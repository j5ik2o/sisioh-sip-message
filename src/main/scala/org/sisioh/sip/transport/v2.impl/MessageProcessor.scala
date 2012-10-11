package org.sisioh.sip.transport.impl

import org.sisioh.sip.util.HostPort
import java.net.InetAddress
import org.sisioh.sip.transaction.SIPTransactionStack

trait MessageProcessor {
  val ipAddress: InetAddress
  val port: Option[Int]
}


abstract class AbstractMessageProcessor
(val ipAddress: InetAddress,
 val port: Option[Int],
 val transport: String,
 transactionStack: SIPTransactionStack) extends MessageProcessor with Runnable {


  val sentBy: String
  val sentByHostPort: HostPort
  val savedIpAddress: String

  def sipStack: SIPTransactionStack = transactionStack

  def run() {

  }

}
