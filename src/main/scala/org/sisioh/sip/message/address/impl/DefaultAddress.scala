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
import org.sisioh.sip.util._
import org.sisioh.sip.core.{GenericObject, Separators}
import net.liftweb.json.ext.EnumNameSerializer
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

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

object DefaultAddressDecoder extends DefaultAddressDecoder

class DefaultAddressDecoder extends SIPDecoder[DefaultAddress] with DefaultAddressParser {
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

object DefaultAddressEncoder extends SIPEncoder[DefaultAddress] {

  def encode(model: DefaultAddress, builder: StringBuilder) = {
    if (model.isWildcard) {
      builder.append('*')
    } else {
      model.displayName.foreach {
        dn =>
          builder.append(Separators.DOUBLE_QUOTE)
            .append(dn)
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

object DefaultAddressJsonDecoder extends JsonDecoder[DefaultAddress] {

  def decode(json: JsonAST.JValue) = {
//    println("json = ", json.toString)
    val JString(uriType) = json \ "uriType"
    val uri = uriType match {
      case "sip" => SipUriJsonDecoder.decode(json \ "uri")
      case "generic" => DefaultGenericURIJsonDecoder.decode(json \ "uri")
      case "wildcard" => WildCardURI
    }
    val displayNameOpt = (json \ "displayName").toOpt.map {
      _.asInstanceOf[JString].s
    }
    val JInt(addressType) = json \ "addressType"
    DefaultAddress(uri, displayNameOpt, Some(AddressType(addressType.toInt)))
  }

}

object DefaultAddressJsonEncoder extends JsonEncoder[DefaultAddress] {

  def encode(model: DefaultAddress) = {
    implicit val formats = net.liftweb.json.DefaultFormats + new EnumNameSerializer(AddressType)
    val uriType = model.uri match {
      case uri: SipUri => "sip"
      case uri: DefaultGenericURI => "generic"
      case uri: WildCardURI => "wildcard"
    }
    ("uriType" -> uriType) ~
      ("uri" -> model.uri.encodeAsJValue()) ~
      ("displayName" -> model.displayName) ~
      ("addressType" -> JInt(model.addressType.id))
  }
}

object DefaultAddress {

  def apply(uri: GenericURI,
            displayName: Option[String] = None,
            addressTypeParam: Option[AddressType.Value] = None): DefaultAddress =
    new DefaultAddress(uri, displayName, addressTypeParam)

  def unapply(defaultAddress: DefaultAddress): Option[(GenericURI, Option[String], AddressType.Value)] =
    Some(defaultAddress.uri, defaultAddress.displayName, defaultAddress.addressType)

  def decode(source: String) = DefaultAddressDecoder.decode(source)

  def decodeFromJson(source: String) = DefaultAddressJsonDecoder.decode(source)

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

  def encode(builder: StringBuilder) =
    DefaultAddressEncoder.encode(this, builder)

  def encodeAsJValue() = DefaultAddressJsonEncoder.encode(this)

  override def hashCode() =
    31 * uri.## + 31 * displayName.## + 31 * addressType.##

  override def equals(obj: Any) = obj match {
    case that: DefaultAddress =>
      uri == that.uri &&
        displayName == that.displayName &&
        addressType == that.addressType
    case _ => false
  }

  override def toString = encode()

}
