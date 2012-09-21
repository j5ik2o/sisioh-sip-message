package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{Address, URI}
import org.sisioh.sip.util.{Decoder, ParserBase}
import org.sisioh.sip.core.{GenericObject, Separators}

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

object DefaultAddressDecoder {
  def apply() = new DefaultAddressDecoder
}

class DefaultAddressDecoder extends Decoder with DefaultAddressParser {
  def decode(source: String) = decodeTarget(source, defaultAddress)
}

trait DefaultAddressParser extends ParserBase with SipUriParser {

  lazy val defaultAddress: Parser[DefaultAddress] = "*" ^^ {
    _ => DefaultAddress(WildCardURI, None, Some(AddressType.WILD_CARD))
  } | opt(Separators.DOUBLE_QUOTE ~> DISPLAY_NAME <~ Separators.DOUBLE_QUOTE) ~ (opt(Separators.LESS_THAN) ~> sipuri <~ opt(Separators.GREATER_THAN)) ^^ {
    case displayNameOpt ~ sipuri =>
      DefaultAddress(sipuri, displayNameOpt)
  }

  lazy val DISPLAY_NAME: Parser[String] = rep(token) ^^ {
    _.mkString
  }

}

object DefaultAddress {

  def apply(uri: GenericURI,
            displayName: Option[String] = None,
            addressTypeParam: Option[AddressType.Value] = None): DefaultAddress =
    new DefaultAddress(uri, displayName, addressTypeParam)

  def decode(source: String) = DefaultAddressDecoder().decode(source)

  def fromURI(uri: URI, displayName: Option[String] = None,
              addressTypeParam: Option[AddressType.Value] = None): DefaultAddress =
    new DefaultAddress(uri.asInstanceOf[GenericURI], displayName, addressTypeParam)

}

/**
 * [[org.sisioh.sip.message.address.Address]]のデフォルト実装
 *
 * @param uri
 * @param displayNameParam
 * @param addressTypeParam
 */
class DefaultAddress
(val uri: GenericURI,
 displayNameParam: Option[String] = None,
 addressTypeParam: Option[AddressType.Value] = None) extends Address with GenericObject {

  val displayName = if (uri.isInstanceOf[WildCardURI]) None else displayNameParam

  val addressType: AddressType.Value = addressTypeParam.getOrElse {
    if (uri.isInstanceOf[WildCardURI]) AddressType.WILD_CARD else AddressType.NAME_ADDRESS
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
      //      println("uri", uri, that.uri, uri == that.uri)
      //      println("display", displayName, that.displayName, displayName == that.displayName)
      //      println("addressType", addressType, that.addressType, addressType == that.addressType)
      uri == that.uri &&
        displayName == that.displayName &&
        addressType == that.addressType
    case _ => false
  }

  override def toString = encode()
}
