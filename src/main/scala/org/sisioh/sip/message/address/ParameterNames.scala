package org.sisioh.sip.message.address

/**
 * Created with IntelliJ IDEA.
 * User: junichi_kato
 * Date: 12/09/26
 * Time: 18:33
 * To change this template use File | Settings | File Templates.
 */
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