package org.sisioh.sip.transaction.v1

import java.util.{Timer, TimerTask}
import org.sisioh.sip.message.impl.SIPMessage
import org.sisioh.sip.util.ErrorReason
import org.sisioh.sip.transport.v1.Transport


object Transaction {
  /**
   * T1値
   */
  val T1 = 500L

  /**
   * TimerB
   */
  val TIMER_B = 64 * T1

  /**
   * TimerF
   */
  val TIMER_F = 64 * T1

  /**
   * TimerH
   */
  val TIMER_H = 64 * T1;
}

abstract class Transaction extends TimerTask {

  private var _currentState: TransactionState = _
  private var _currentSIPMessage: SIPMessage = _
  private var timer: Option[Timer] = None

  val transport: Transport
  val transactionListener: TransactionListener
  val isCancelTransaction: Boolean = false

  def onReceived(sipMessage: SIPMessage): Unit = {
    _currentSIPMessage = sipMessage
    _currentState.onReceived(transport, sipMessage)
  }

  def sendMessage(sipMessage: SIPMessage): Unit = {
    _currentSIPMessage = sipMessage
    _currentState.sendMessage(sipMessage)
  }

  def onError(reason: ErrorReason.Value): Unit = {
    _currentState.onError(reason)
  }

  def changeState(newState: TransactionState): Unit = synchronized {
    _currentState.onLeave
    _currentState = newState
    _currentState.onEnter
  }

  def currentState = _currentState.state
  def currentSIPMessage = _currentSIPMessage


  def terminated: Unit = {
    transactionListener.destroyTransaction(this)
  }

  def destroy: Unit =
    transport.close()

  def startTimer(ms: Long) = {
    timer.map {
      t =>
        t.cancel()
        t
    }.getOrElse(new Timer).schedule(this, ms)
  }

  def stopTimer: Unit = {
    timer.foreach {
      t =>
        t.cancel()
        timer = None
    }
  }


  def run() {
    _currentState.onTimeout
  }
}
