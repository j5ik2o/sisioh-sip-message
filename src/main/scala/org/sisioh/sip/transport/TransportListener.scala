package org.sisioh.sip.transport

import org.sisioh.sip.message.impl.SIPMessage

trait TransportListener {
  /**
   * パケットが届いたときに呼ばれる
   *
   * @param transport 受信したトランスポート
   * @param sipMessage 受信したSIPメッセージ
   */
  def onReceivedFromTransport(transport: Transport, sipMessage: SIPMessage[Any])

  /**
   * 接続が確立されたときに呼ばれる
   *
   * @param transport 接続されたトランスポート
   */
  def onConnected(transport: Transport);

  /**
   * 接続が切断されたときに呼ばれる。
   *
   * @param transport 切断されたトランスポート
   */
  def onDisconnected(transport: Transport);
}
