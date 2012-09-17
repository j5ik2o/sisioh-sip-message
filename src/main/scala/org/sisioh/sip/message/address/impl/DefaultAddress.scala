package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{Address, URI}
import org.sisioh.sip.util.{Encoder, Encodable}
import org.sisioh.sip.core.Separators

object AddressType extends Enumeration {
  val NAME_ADDRESS, ADDRESS, WILD_CARD = Value
}

object DefaultAddress {

  implicit object DefaultDefaultAddressEncoder extends Encoder[DefaultAddress] {
    def encode(model: DefaultAddress, builder: StringBuilder) = {
      if (model.addressType == AddressType.WILD_CARD) {
        builder.append('*')
      } else {
        if (model.displayName.isDefined) {
          builder.append(Separators.DOUBLE_QUOTE)
            .append(model.displayName)
            .append(Separators.DOUBLE_QUOTE)
            .append(Separators.SP)
        }
        if (model.addressType == AddressType.NAME_ADDRESS || model.displayName.isDefined)
          builder.append(Separators.LESS_THAN)
        model.uri.encode(builder)
        if (model.addressType == AddressType.NAME_ADDRESS || model.displayName.isDefined)
          builder.append(Separators.GREATER_THAN)
      }
      builder
    }
  }

}

case class DefaultAddress
(uri: SipUri,
 addressTypeParam: Option[AddressType.Value],
 displayName: Option[String] = None) extends Address with Encodable[DefaultAddress] {

  val addressType: AddressType.Value = addressTypeParam.getOrElse {
    AddressType.NAME_ADDRESS
  }

  val isWildcard: Boolean = addressType == AddressType.WILD_CARD


  def removeDisplayName: DefaultAddress =
    DefaultAddress(uri, None)

  def hasDisplayName: Boolean = displayName.isDefined

  val isSipURI = uri.isSipURI

  def encode(builder: StringBuilder) = null
}
