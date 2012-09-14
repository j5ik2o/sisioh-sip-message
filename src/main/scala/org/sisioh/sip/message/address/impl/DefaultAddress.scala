package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{Address, URI}

object AddressType extends Enumeration {
  val NAME, ADDRESS, WILD_CARD = Value
}

case class DefaultAddress
(uri: SipUri,
 addressTypeParam: Option[AddressType.ValueSet],
 displayName: Option[String] = None) extends Address {

  val addressType = addressTypeParam.getOrElse {
    AddressType.NAME
  }

  val isWildcard: Boolean = addressType == AddressType.WILD_CARD


  //  def removeDisplayName: DefaultAddress =
  //    DefaultAddress(uri, None, isWildcard)

  def hasDisplayName: Boolean = displayName.isDefined

  val isSipURI = uri.isSipURI

}
