package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{Address, URI}
import org.sisioh.sip.util.{Encoder, Encodable}
import org.sisioh.sip.core.Separators

object AddressType extends Enumeration {
  val NAME_ADDRESS, ADDRESS, WILD_CARD = Value
}

object DefaultAddress {

}

case class DefaultAddress
(uri: SipUri,
 displayName: Option[String] = None,
 addressTypeParam: Option[AddressType.Value] = None) extends Address with Encodable[DefaultAddress] {

  val addressType: AddressType.Value = addressTypeParam.getOrElse {
    AddressType.NAME_ADDRESS
  }

  val isWildcard: Boolean = addressType == AddressType.WILD_CARD

  def removeDisplayName: DefaultAddress =
    DefaultAddress(uri, None, Some(addressType))

  def hasDisplayName: Boolean = displayName.isDefined

  val isSipURI = uri.isSipURI

  def encode(builder: StringBuilder) = {
    if (isWildcard) {
      builder.append('*')
    } else {
      if (displayName.isDefined) {
        builder.append(Separators.DOUBLE_QUOTE)
          .append(displayName.get)
          .append(Separators.DOUBLE_QUOTE)
          .append(Separators.SP)
      }
      if (addressType == AddressType.NAME_ADDRESS || displayName.isDefined)
        builder.append(Separators.LESS_THAN)
      uri.encode(builder)
      if (addressType == AddressType.NAME_ADDRESS || displayName.isDefined)
        builder.append(Separators.GREATER_THAN)
    }
    builder
  }

}
