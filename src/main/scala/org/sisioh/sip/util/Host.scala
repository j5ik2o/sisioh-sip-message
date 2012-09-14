package org.sisioh.sip.util

import java.net.{Inet4Address, Inet6Address, InetAddress}
import java.util.regex.Pattern

object AddressType extends Enumeration {
  val HOST_NAME, IPV4_ADDRESS, IPV6_ADDRESS = Value
}

/**
 * [[org.sisioh.sip.util.Host]]のコンパニオンオブジェクト。
 *
 * @author j5ik2o
 */
object Host {

  def apply(hostNameOrIpAddressParam: String, addressTypeParam: Option[AddressType.Value] = None) = new Host(hostNameOrIpAddressParam, addressTypeParam)

  def unapply(host: Host): Option[(String, AddressType.Value)] = Some(host.hostNameOrIpAddressParam, host.addressType)

  /**
   * デフォルトのエンコーダー
   */
  implicit object DefaultHostEncoder extends Encoder[Host] {
    def encode(model: Host, builder: StringBuilder) = {
      val encodedModel = if (model.addressType == AddressType.IPV6_ADDRESS
        && !model.isIPv6Reference(model.hostNameOrIpAddressParam)) {
        "[" + model.hostNameOrIpAddressParam + "]"
      } else {
        model.hostNameOrIpAddressParam
      }
      new StringBuilder(encodedModel)
    }
  }

}

/**
 * ホストを表す値オブジェクト。
 *
 * @param hostNameOrIpAddressParam ホスト名もしくはIPアドレス
 * @param addressTypeParam [[org.sisioh.sip.util.AddressType.Value]]
 */
class Host(val hostNameOrIpAddressParam: String, addressTypeParam: Option[AddressType.Value] = None)
  extends Encodable[Host] {

  val inetAddress: InetAddress =
    InetAddress.getByName(hostNameOrIpAddressParam)

  private val v4Partial = "25[0-5]|2[0-4]\\d|[0-1]?\\d?\\d"
  private val v4Pattern = Pattern.compile("(" + v4Partial + ")(\\.(" + v4Partial + ")){3}");
  private val v6Partial = "[0-9a-f]{1,4}"
  private val v6PattenBase = "(" + v6Partial + ")(:(" + v6Partial + "))"
  private val v6Pattern = Pattern.compile(v6PattenBase + "{7}")

  private def isIpV4AddressText(text: String) = v4Pattern.matcher(text).matches()

  private def isIpV6AddressText(text: String) = v6Pattern.matcher(text).matches()

  val addressType = addressTypeParam.getOrElse {
    hostNameOrIpAddressParam match {
      case s if isIpV4AddressText(s) => AddressType.IPV4_ADDRESS
      case s if isIpV6AddressText(s) => AddressType.IPV6_ADDRESS
      case _ => AddressType.HOST_NAME
    }
  }

  val isHostName = addressType == AddressType.HOST_NAME

  val isIpAddress = addressType != AddressType.HOST_NAME

  val hostName: Option[String] = addressType match {
    case AddressType.IPV4_ADDRESS | AddressType.IPV6_ADDRESS => None
    case _ => Some(hostNameOrIpAddressParam)
  }

  val ipAddress: Option[String] = addressType match {
    case AddressType.IPV4_ADDRESS | AddressType.IPV6_ADDRESS => Some(hostNameOrIpAddressParam)
    case _ => None
  }

  val resolvedIpAddress: String = addressType match {
    case AddressType.IPV4_ADDRESS | AddressType.IPV6_ADDRESS => hostNameOrIpAddressParam
    case AddressType.HOST_NAME => inetAddress.getHostAddress
  }

  override def equals(other: Any) = other match {
    case that: Host =>
      this.hostNameOrIpAddressParam == that.hostNameOrIpAddressParam && this.addressType == that.addressType
    case _ => false
  }

  override def hashCode: Int =
    31 * hostNameOrIpAddressParam.## + 31 * addressType.##

  override def toString = "Host(%s, %s)".format(inetAddress.getHostAddress, inetAddress.getHostName)

  private def isIPv6Reference(address: String): Boolean =
    address.charAt(0) == '[' && address.charAt(address.length() - 1) == ']'

}
