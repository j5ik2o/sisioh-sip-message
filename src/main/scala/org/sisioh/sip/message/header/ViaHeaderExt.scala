package org.sisioh.sip.message.header

trait ViaHeaderExt extends ViaHeader {
  val sentByField: String
  val sentByProtocolField: String
}
