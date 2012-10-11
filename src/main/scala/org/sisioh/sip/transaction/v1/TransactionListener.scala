package org.sisioh.sip.transaction.v1

import org.sisioh.sip.transport.TransportListener
import org.sisioh.sip.message.impl.SIPMessage
import org.sisioh.sip.util.ErrorReason

/**
 * 上位層(トランザクションユーザ)との接続を行うクラスのインタフェース。
 * トランザクションユーザはトランスポートとの接続も必要なため、TransportListenerをextendsしている。
 */
trait TransactionListener extends TransportListener {

  /**
   * TransactionからPIPメッセージを受け取った場合に呼ばれる。
   *
   * @param receivedSIPMessage 受信したPIPメッセージ
   */
  def onReceivedFromTransaction(receivedSIPMessage: SIPMessage): Unit

  /**
   * 何らかのエラーが発生した(タイムアウト、トランスポート,etc.)
   *
   * @param errorTransaction エラーが発生したトランザクション
   * @param reason エラー理由
   */
  def onError(errorTransaction: Transaction, reason: ErrorReason.Value): Unit

  /**
   * 何らかのエラーが発生した(タイムアウト、トランスポート,etc.)
   * トランザクションが作成される前に発生したエラー。
   *
   * @param errorMessage エラーが発生したメッセージ
   * @param reason エラー理由
   */
  def onError(errorMessage: SIPMessage, reason: ErrorReason.Value): Unit

  /**
   * トランザクションを終了する。
   * トランザクションの管理に関する処理は、トランザクションユーザの仕事であるため、ここに用意されている。
   *
   * @param transaction
	 * 削除するトランザクション
   */
  def destroyTransaction(transaction: Transaction): Unit


}
