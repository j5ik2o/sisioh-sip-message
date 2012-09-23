package org.sisioh.sip.message.address.impl

/*
 * Copyright 2012 Sisioh Project and others. (http://www.sisioh.org/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */

import org.sisioh.sip.message.address.{Address, URI}
import org.sisioh.sip.util.{NameValuePair, Encoder, Decoder, ParserBase}
import org.sisioh.sip.core.{GenericObject, Separators}
import net.liftweb.json.ext.{EnumNameSerializer, EnumSerializer}

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

  lazy val defaultAddress: Parser[DefaultAddress] = elem('*') ^^ {
    _ =>
      DefaultAddress(WildCardURI, None, Some(AddressType.WILD_CARD))
  } | nameAddrToDefaultAddress | addrSpecToDefaultAddress

  lazy val nameAddrToDefaultAddress = nameAddr ^^ {
    case dn ~ uri =>
      DefaultAddress(uri.asInstanceOf[GenericURI], dn)
  }

  lazy val addrSpecToDefaultAddress = addrSpec ^^ {
    case uri =>
      DefaultAddress(uri.asInstanceOf[GenericURI])
  }

  lazy val nameAddr = opt(displayName) ~ (LAQUOT ~> addrSpec <~ RAQUOT)
  lazy val addrSpec: Parser[URI] = SIP_URI | SIPS_URI | absoluteURI

  lazy val displayName: Parser[String] = rep1(token ~ LWS ^^ {
    case f ~ s => f + s
  }) ^^ {
    case t =>
      t.mkString
  } | quotedString

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

  object JsonEncoder extends Encoder[DefaultAddress] {
    def encode(model: DefaultAddress, builder: StringBuilder) = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      implicit val formats = net.liftweb.json.DefaultFormats + new EnumNameSerializer(AddressType)
      val json = ("uri" -> parse(model.uri.encodeByJson())) ~
        ("displayName" -> model.displayName) ~
        ("addressType" -> JInt(model.addressType.id))
      builder.append(compact(render(json)))
    }
  }

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

  def encodeByJson(builder: StringBuilder) = encode(builder, DefaultAddress.JsonEncoder)
}
