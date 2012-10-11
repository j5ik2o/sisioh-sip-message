package org.sisioh.sip.transport.v1

import org.sisioh.sip.message.impl.{SIPMessageDecoder, SIPMessage}
import java.net.Socket
import annotation.tailrec
import java.io.ByteArrayOutputStream
import org.sisioh.sip.util.ParseException

object TCPTransport {

  def apply(socket: Socket, receiveTransportListener: Option[TransportListener]): TCPTransport =
    new TCPTransport(socket, receiveTransportListener)

  /**
   * 相手と接続する。
   *
   * @param peer 相手先のPeer
   */
  def connect(peer: Peer, receiveTransportListener: Option[TransportListener]): TCPTransport =
    apply(new Socket(peer.address, peer.port), receiveTransportListener)

}


class TCPTransport(val socket: Socket, val receiveTransportListener: Option[TransportListener]) extends Transport {

  protected val inputStream = socket.getInputStream
  protected val outputStream = socket.getOutputStream

  receiveTransportListener.foreach(_.onConnected(this))

  def close() {
    try {
      inputStream.close()
    } catch {
      case _ =>
    }
    try {
      outputStream.close()
    } catch {
      case _ =>
    }
    try {
      socket.close()
    } catch {
      case _ =>
    }
  }

  def sendMessage(sendSIPMessage: SIPMessage) {
    val buf = sendSIPMessage.encodeAsBytes()
//    println("send size = " + buf.size)
    outputStream.write(buf, 0, buf.size)
  }

  protected val contentLengthRegex = "Content-Length\\s*:\\s*(\\d+)".r
  protected val delimiters = List('\r', '\n', '\r', '\n').reverse

  def recieveMessage() = {
    @tailrec
    def readBytes(reading: List[Byte] = List.empty): Array[Byte] = {
      inputStream.read() match {
        case -1 =>
          reading.reverse.toArray
        case n =>
          val bytes = n.toByte :: reading
          if (bytes.indexOfSlice(delimiters) != -1) {
            bytes.reverse.toArray
          } else {
            readBytes(bytes)
          }
      }
    }
    val headersAsBytes = readBytes()
    if (headersAsBytes.isEmpty) {
      None
    } else {
      val bout = new ByteArrayOutputStream()
      bout.write(headersAsBytes)
      val headersAsString = new String(headersAsBytes, "UTF-8")
      val m = contentLengthRegex.findFirstMatchIn(headersAsString)
      if (m.isDefined) {
        val cl = m.get.group(1)
        val contentBytes = new Array[Byte](cl.toInt)
        inputStream.read(contentBytes) match {
          case len if len > 0 =>
//            println("c = {" + new String(contentBytes) + "}")
            bout.write(contentBytes, 0, len)
          case _ =>
            throw new ParseException()
        }
      }
      val messageAsBytes = bout.toByteArray
//      println("receive size = " + messageAsBytes.size)
      Some(SIPMessageDecoder.decode(messageAsBytes))
    }
  }

  override def hashCode() = 31 * socket.## + 31 * receiveTransportListener.##

  override def equals(obj: Any) = obj match {
    case that: TCPTransport =>
      socket == socket &&
        receiveTransportListener == receiveTransportListener
    case _ =>
      false
  }

  override def toString = "TCPTransport(%s,%s)".format(socket, receiveTransportListener)

}
