package org.sisioh.sip.util

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

import java.net.InetAddress
import java.util.regex.Pattern
import org.sisioh.sip.core.GenericObject

/**
 * アドレスの種別を表す列挙型。
 */
object AddressType extends Enumeration {
  val HOST_NAME, IPV4_ADDRESS, IPV6_ADDRESS = Value
}

object HostDecoder {

  def apply() = new HostDecoder

}

class HostDecoder extends Decoder with HostParser {

  def decode(source: String): Host = decodeTarget(source, hostToModel)

}

trait HostParser extends ParserBase {

  lazy val hostToModel: Parser[Host] = host ^^ {
    h => Host(h)
  }

}


/**
 * [[org.sisioh.sip.util.Host]]のコンパニオンオブジェクト。
 *
 * @author j5ik2o
 */
object Host {

  def apply(hostNameOrIpAddress: String, addressTypeParam: Option[AddressType.Value] = None) = new Host(hostNameOrIpAddress, addressTypeParam)

  def decode(source: String) = HostDecoder().decode(source)

  def unapply(host: Host): Option[(String, AddressType.Value)] = Some(host.hostNameOrIpAddress, host.addressType)

  val v4Partial = "25[0-5]|2[0-4]\\d|[0-1]?\\d?\\d"
  val v4Pattern = Pattern.compile("(" + v4Partial + ")(\\.(" + v4Partial + ")){3}")
  val v6Partial = "[0-9a-f]{1,4}"
  val v6PattenBase = "(" + v6Partial + ")(:(" + v6Partial + "))"
  val v6Pattern = Pattern.compile(v6PattenBase + "{7}")

  object JsonEncoder extends Encoder[Host] {
    def encode(model: Host, builder: StringBuilder) = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val json = ("hostNameOrIpAddress" -> model.hostNameOrIpAddress) ~
        ("addressTypeParam" -> model.addressType.id)
      builder.append(compact(render(json)))
    }

  }

}

/**
 * ホストを表す値オブジェクト。
 *
 * @param hostNameOrIpAddress ホスト名もしくはIPアドレス
 * @param addressTypeParam [[org.sisioh.sip.util.AddressType.Value]]
 */
class Host(val hostNameOrIpAddress: String, addressTypeParam: Option[AddressType.Value] = None)
  extends GenericObject {

  val inetAddress: InetAddress =
    InetAddress.getByName(hostNameOrIpAddress)


  private def isIpV4AddressText(text: String) = Host.v4Pattern.matcher(text).matches()

  private def isIpV6AddressText(text: String) = Host.v6Pattern.matcher(text).matches()

  val addressType = addressTypeParam.getOrElse {
    hostNameOrIpAddress match {
      case s if isIpV4AddressText(s) => AddressType.IPV4_ADDRESS
      case s if isIpV6AddressText(s) => AddressType.IPV6_ADDRESS
      case _ => AddressType.HOST_NAME
    }
  }

  val isHostName = addressType == AddressType.HOST_NAME

  val isIpAddress = addressType != AddressType.HOST_NAME

  val hostName: Option[String] = addressType match {
    case AddressType.IPV4_ADDRESS | AddressType.IPV6_ADDRESS => None
    case _ => Some(hostNameOrIpAddress)
  }

  val ipAddress: Option[String] = addressType match {
    case AddressType.IPV4_ADDRESS | AddressType.IPV6_ADDRESS => Some(hostNameOrIpAddress)
    case _ => None
  }

  val resolvedIpAddress: String = addressType match {
    case AddressType.IPV4_ADDRESS | AddressType.IPV6_ADDRESS => hostNameOrIpAddress
    case AddressType.HOST_NAME => inetAddress.getHostAddress
  }

  override def equals(other: Any) = other match {
    case that: Host =>
      this.hostNameOrIpAddress == that.hostNameOrIpAddress &&
        this.addressType == that.addressType
    case _ => false
  }

  override def hashCode: Int =
    31 * hostNameOrIpAddress.## + 31 * addressType.##

  override def toString = encode()

  private def isIPv6Reference(address: String): Boolean =
    address.charAt(0) == '[' && address.charAt(address.length() - 1) == ']'

  def encode(builder: StringBuilder) = {
    val encodedModel = if (addressType == AddressType.IPV6_ADDRESS
      && !isIPv6Reference(hostNameOrIpAddress)) {
      "[" + hostNameOrIpAddress + "]"
    } else {
      hostNameOrIpAddress
    }
    builder.append(encodedModel)
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, Host.JsonEncoder)
}
