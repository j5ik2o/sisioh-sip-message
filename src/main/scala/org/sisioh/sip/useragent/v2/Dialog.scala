package org.sisioh.sip.useragent.v2

import org.sisioh.sip.message.address.Address
import org.sisioh.sip.message.header.CallIdHeader
import org.sisioh.sip.message.header.impl.RecordRoute
import org.sisioh.sip.message.{Response, Request}

trait Dialog {

  def localParty: Address

  def remoteParty: Address

  def dialogId: String

  def callId: CallIdHeader

  def localSequenceNumber: Int

  def localSeqNumber: Long

  def remoteSequenceNumber: Int

  def remoteSeqNumber: Long

  def routeSet: Iterator[RecordRoute]

  def isSecure: Boolean

  def isServer: Boolean

  def incrementLocalSequenceNumber: Unit

  def createRequest(method: String): Request

  def createReliableProvisionalResponse(statusCode: Int)

  def sendRequest(clientTransaction: ClientTransaction): Unit

  def sendReliableProvisionalResponse(relResponse: Response): Unit

  def createPrack(relResponse: Response): Request

  def createAck(cSeq: Long): Request

  def sendAck(ackRequest: Request): Unit

  def state: DialogState

  def delete: Unit

  def firstTransaction: Transaction

  def localTag: String

  def remoteTag: String

  def applicationData: Any

  def terminatedOnBye(terminateFlag: Boolean)
}
