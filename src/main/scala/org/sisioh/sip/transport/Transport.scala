package org.sisioh.sip.transport

import org.sisioh.sip.message.impl.SIPMessage

trait Transport {

  protected val receiveTransportListener: Option[TransportListener]

  /**
   * 相手と接続する。
   *
   * @param peer 相手先のPeer
   */
  def connect(peer: Peer): Unit

  /**
   * 接続を閉じる。
   */
  def close(): Unit

  /**
   * SIPMessageを送信する。
   *
   * @param sendSIPMessage 送信するSIPMessage
   */
  def sendMessage(sendSIPMessage: SIPMessage[Any])

  /**
   * SIPMessageを受信する。
   *
   * @return 受信したSIPMessage
   */
  def recvMessage(): SIPMessage[Any]

}
