package org.sisioh.sip.transport.impl

import org.sisioh.sip.transaction.impl.SIPClientTransaction
import org.sisioh.sip.transaction.SIPTransactionStack
import org.sisioh.sip.message.impl.{MetaData, SIPRequest, SIPMessage}
import java.net.{UnknownHostException, InetAddress}
import org.sisioh.sip.message.address.Hop
import org.sisioh.sip.util.{Host, HostPort}
import org.sisioh.sip.message.header.impl.Via
import org.sisioh.sip.message.header.Protocol
import grizzled.slf4j.Logging

trait MessageChannel extends Logging {

  protected val useCount: Int

  def uncache: Unit = {}

  @transient
  protected val messageProcessor: MessageProcessor

  def close: Unit

  def transport: String

  def isReliable: Boolean

  def isSecure: Boolean

  def sendMessage(sipMessage: SIPMessage): Unit

  def peerAddress: String

  protected def peerInetAddress: InetAddress

  protected def peerProtocol: String

  def peerPort: Int

  def peerPacketSourcePort: Int

  def peerPackegtSourceAddress: InetAddress

  def key: String

  def viaHost: String

  def viaPort: Int

  protected def sendMessage(message: Array[Byte], receiverAddress: InetAddress, receiverPort: Int, reconnectFlag: Boolean)

  def host: String = messageProcessor.ipAddress.getHostAddress

  def port: Option[Int] = messageProcessor.port

  def sendMessage(sipMessage: SIPMessage, hop: Hop) = {
    val time = System.currentTimeMillis()
    val hopAddr = InetAddress.getByName(hop.host)

  }

  def sendMessage(sipMessage: SIPMessage, receiverAddress: InetAddress, receiverPort: Int): SIPMessage = {
    val time = System.currentTimeMillis()
    val bytes = sipMessage.encodeAsBytes(Some(transport))
    sendMessage(bytes, receiverAddress, receiverPort, sipMessage.isInstanceOf[SIPRequest])
    val metaData = MetaData(
      remoteAddress = Some(receiverAddress),
      remotePort = Some(receiverPort),
      localAddress = Some(messageProcessor.ipAddress),
      localPort = port,
      applicationData = None
    )
    val result = sipMessage.withMetaData(Some(metaData))
    logMessage(sipMessage, receiverAddress, receiverPort, time)
    result
  }

  def logMessage(sipMessage: SIPMessage, address: InetAddress, port: Int, time: Long) = {

  }


  def rawIpSourceAddress: String = {
    try {
      val sourceInetAddress = InetAddress.getByName(peerAddress)
      sourceInetAddress.getHostAddress
    } catch {
      case ex: UnknownHostException =>
        // TODO
        throw ex
    }
  }

  def key(inetAddr: InetAddress, port: Int, transport: String) =
    (transport + ":" + inetAddr.getHostAddress.replaceAll("[\\[\\]]", "") + ":" + port).toLowerCase

  def key(hostPort: HostPort, transport: String) =
    (transport + ":" + hostPort.host.hostName.get.replaceAll("[\\[\\]]", "") + ":" + port).toLowerCase

  def hostPort =
    HostPort(Host(host), Some(viaPort))

  def viaHeader: Via =
    Via(hostPort, Protocol(transport = transport))

  def viaHostPort: HostPort =
    HostPort(Host(host), Some(viaPort))


}
