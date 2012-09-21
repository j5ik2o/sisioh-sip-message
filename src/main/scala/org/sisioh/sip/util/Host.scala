package org.sisioh.sip.util

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

  def decode(source: String): Host = decodeTarget(source, host)

}

trait HostParser extends ParserBase {

  def host: Parser[Host] = hostNameOrIpAddress ^^ {
    h => Host(h)
  }


  lazy val L_BRACKET = """\[""".r
  lazy val R_BRACKET = """\]""".r
  lazy val ipV4Address = ("(" + Host.v4Partial + ")(\\.(" + Host.v4Partial + ")){3}").r
  lazy val ipV6Address = L_BRACKET ~> (Host.v6PattenBase + "{7}").r <~ R_BRACKET
  lazy val hostNameOrIpAddress = HOSTNAME | ipV4Address | ipV6Address
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
}
