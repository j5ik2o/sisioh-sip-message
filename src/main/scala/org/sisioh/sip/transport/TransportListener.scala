package org.sisioh.sip.transport

/*
 * Copyright 2012 Sisioh Project and others. (http://www.sisioh.org/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */

import org.sisioh.sip.message.impl.SIPMessage

trait TransportListener {
  /**
   * パケットが届いたときに呼ばれる
   *
   * @param transport 受信したトランスポート
   * @param sipMessage 受信したSIPメッセージ
   */
  def onReceivedFromTransport(transport: Transport, sipMessage: SIPMessage)

  /**
   * 接続が確立されたときに呼ばれる
   *
   * @param transport 接続されたトランスポート
   */
  def onConnected(transport: Transport)

  /**
   * 接続が切断されたときに呼ばれる。
   *
   * @param transport 切断されたトランスポート
   */
  def onDisconnected(transport: Transport)
}
