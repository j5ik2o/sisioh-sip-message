package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.CallIdHeader

case class CallID(callId: String) extends CallIdHeader {
  val name = CallIdHeader.NAME
  val callIdentity = CallIdentifier(callId)
}
