package org.sisioh.sip.transport.v1

import java.net.ServerSocket
import java.io.IOException

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

object TCPServerTransport {

  def apply(serverSocket: ServerSocket): TCPServerTransport =
    apply(serverSocket, None)

  def apply(serverSocket: ServerSocket, receiveListener: Option[TransportListener]): TCPServerTransport =
    new TCPServerTransport(serverSocket, receiveListener)

  def apply(serverPort: Int): TCPServerTransport =
    apply(new ServerSocket(serverPort), None)

  def apply(serverPort: Int, receiveListener: Option[TransportListener]): TCPServerTransport =
    new TCPServerTransport(new ServerSocket(serverPort), receiveListener)

  def unapply(tcpServerTransport: TCPServerTransport): Option[(ServerSocket, Option[TransportListener])] =
    Some(tcpServerTransport.serverSocket, tcpServerTransport.receiveListener)

}

class TCPServerTransport(val serverSocket: ServerSocket, val receiveListener: Option[TransportListener]) {

  def close() = {
    serverSocket.close()
  }

  def accept() = {
    while (true) {
      import org.sisioh.sip.util.Loan._
      val socket = serverSocket.accept()
      using(TCPTransport(socket, receiveListener)) {
        tcp =>
          receiveListener.foreach(_.onConnected(tcp))
          tcp.doProcess
          tcp
      }
    }
  }

  override def hashCode() = 31 * serverSocket.## + 31 * receiveListener.##

  override def equals(obj: Any) = obj match {
    case that: TCPServerTransport =>
      serverSocket == that.serverSocket &&
        receiveListener == that.receiveListener
    case _ =>
      false
  }

  override def toString = "TCPServerTransport(%s,%s)".format(serverSocket, receiveListener)
}
