package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{Address, URI}
import org.sisioh.sip.util.{Encoder, Encodable}
import org.sisioh.sip.core.{GenericObject, Separators}
import util.parsing.combinator.RegexParsers

/**
 * アドレスの種別を表す列挙型。
 */
object AddressType extends Enumeration {
  /** 名前 */
  val NAME_ADDRESS,

  /** アドレス */
  ADDRESS,

  /** ワイルドカード */
  WILD_CARD = Value
}

trait DefaultAddressParser extends RegexParsers {
  //def defaultAddress: Parser[DefaultAddress] = "*" ||| opt(DISPLAY_NAME) ~ opt(Separators.LESS_THAN) ~ URI ~ opt(Separators.GREATER_THAN)

  lazy val DISPLAY_NAME = """[a-zA-Z.]""".r

}

object DefaultAddress {

  def apply(uri: AbstractGenericURI,
            displayName: Option[String] = None,
            addressTypeParam: Option[AddressType.Value] = None): DefaultAddress =
    new DefaultAddress(uri, displayName, addressTypeParam)

  def fromURI(uri: URI, displayName: Option[String] = None,
            addressTypeParam: Option[AddressType.Value] = None): DefaultAddress =
    new DefaultAddress(uri.asInstanceOf[AbstractGenericURI], displayName, addressTypeParam)

}

/**
 * [[org.sisioh.sip.message.address.Address]]のデフォルト実装
 *
 * @param uri
 * @param displayName
 * @param addressTypeParam
 */
class DefaultAddress
(val uri: AbstractGenericURI,
 val displayName: Option[String] = None,
 addressTypeParam: Option[AddressType.Value] = None) extends Address with GenericObject {

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

  override def hashCode() = super.hashCode()

  override def equals(obj: Any) = obj match {
    case that: DefaultAddress =>
      uri == that.uri && displayName == that.displayName && addressType == that.addressType
    case _ => false
  }

  override def toString = encode()
}
