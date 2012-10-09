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
import org.sisioh.sip.util.ParseException
import java.io.IOException
import annotation.tailrec

trait Transport extends Runnable {

  protected val receiveTransportListener: Option[TransportListener]
  protected val recvMonitorThread: Thread = new Thread(this)


  /**
   * 接続を閉じる。
   */
  def close(): Unit

  /**
   * SIPMessageを送信する。
   *
   * @param sendSIPMessage 送信するSIPMessage
   */
  def sendMessage(sendSIPMessage: SIPMessage)

  /**
   * SIPMessageを受信する。
   *
   * @return 受信したSIPMessage
   */
  def recieveMessage(): Option[SIPMessage]

  def doProcess = {
    @tailrec
    def process0(): Unit = {
      val r = try {
        val msg = recieveMessage()
        msg.foreach {
          m =>
            receiveTransportListener.foreach(_.onReceivedFromTransport(this, m))
        }
        msg
      } catch {
        case ex: ParseException =>
          ex.printStackTrace()
          None
      }
      r match {
        case None => ()
        case Some(_) =>
          process0
      }
    }

    process0
  }

  def startMonitoring() {
    recvMonitorThread.start()
  }

  def run() {
    try {
      doProcess
    } catch {
      case ex: IOException =>
        receiveTransportListener.foreach(_.onDisconnected(this))
      case ex: Exception =>
        ex.printStackTrace()
    }
  }


}
