package org.sisioh.sip.message.address

trait ParameterNames {
  val SIP_URI_SCHEME = "sip"
  val SIPS_URI_SCHEME = "sips"
  val TEL_URI_SCHEME = "tel"
  val POSTDIAL = "postd"
  val PHONE_CONTEXT_TAG = "context-tag"
  val ISUB = "isub"
  val PROVIDER_TAG = "provider-tag"
  val UDP = NetObject.UDP
  val TCP = NetObject.TCP
  val TLS = NetObject.TLS
}

object ParameterNames extends ParameterNames