package org.sisioh.sip.transaction.v1

import org.sisioh.sip.transport.v1.Transport
import org.sisioh.sip.message.impl.SIPMessage
import org.sisioh.sip.util.ErrorReason

/**
 * Created with IntelliJ IDEA.
 * User: junichi_kato
 * Date: 12/10/10
 * Time: 18:14
 * To change this template use File | Settings | File Templates.
 */
trait TransactionState {

  protected val transaction: Transaction

  def onEnter: Unit

  def onLeave: Unit

  def onReceived(receivedTransport: Transport, receivedSIPMessage: SIPMessage): Unit

  def sendMessage(sipMessage: SIPMessage): Unit

  def onError(reason: ErrorReason.Value): Unit

  def state: String

  def onTimeout: Unit = {}

  def startTimer(ms: Long):Unit = {
    transaction.startTimer(ms)
  }


}

object TransactionState {

  /**
   * TryingState
   */
  val TRYING = "TRYING"

  /**
   * CallingState
   */
  val CALLING = "CALLING"

  /**
   * ProceedingState
   */
  val PROCEEDING = "PROCEEDING"

  /**
   * CompletedState
   */
  val COMPLETED = "COMPLETED"

  /**
   * TreminatedState
   */
  val TERMINATED = "TERMINATED"

}