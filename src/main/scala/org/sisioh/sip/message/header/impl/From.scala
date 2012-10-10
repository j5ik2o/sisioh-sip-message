package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.{ParameterNames, FromHeader}
import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl.{DefaultAddressJsonDecoder, DefaultAddressParser, AddressType, DefaultAddress}
import org.sisioh.sip.core.Separators
import net.liftweb.json._
import scala.Some


object FromDecoder extends FromDecoder

class FromDecoder extends SIPDecoder[From] with FromParser {
  def decode(source: String): From = decodeTarget(source, fromWithCrLfOpt)
}

trait FromParser extends ToOrFromParser with DefaultAddressParser {
  lazy val fromWithCrLfOpt: Parser[From] = from <~ opt(CRLF)

  lazy val from: Parser[From] = ("From" | "f") ~> HCOLON ~> ((nameAddrToDefaultAddress | addrSpecToDefaultAddress) ~ rep(SEMI ~> toParam)) ^^ {
    case da ~ toParams =>
      From(da, None, NameValuePairList.fromValues(toParams))
  }

}



object FromJsonDecoder extends ToFromJsonDecoder[From] {

  val headerName = FromHeader.NAME

  protected def createInstance(address: DefaultAddress, parameters: NameValuePairList) =
    From(address, None, parameters)

}

object FromJsonEncoder extends ToFromJsonEncoder[From]


object From {

  def apply
  (address: DefaultAddress,
   tag: Option[String] = None,
   parameters: NameValuePairList = NameValuePairList()) = new From(address, tag, parameters)

  def fromTo(to: To) =
    new From(to.address)

  def unapply(from: From): Option[(DefaultAddress, NameValuePairList)] =
    Some(from.address, from.parameters)

  def decode(source: String) = FromDecoder.decode(source)

  def decodeFromJson(source: String) = FromJsonDecoder.decode(source)


}

/**
 * Fromヘッダを表す値オブジェクト。
 *
 * @param address
 * @param tagParam
 * @param parametersParam
 */
class From
(val address: DefaultAddress,
 tagParam: Option[String] = None,
 parametersParam: NameValuePairList = NameValuePairList())
  extends ToOrFromHeader with FromHeader {

  type ParametersHeaderType = From

  val headerName = FromHeader.NAME
  val name = headerName


  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  def hasTag: Boolean = hasParameter(ParameterNames.TAG)

  def tag = {
    (tagParam, parametersParam.getValueAsString(ParameterNames.TAG)) match {
      case (Some(tp), _) => Some(tp)
      case (_, Some(tp)) => Some(tp)
      case _ => None
    }
  }

  val parameters = tagParam.map(t => parametersParam.add(ParameterNames.TAG, t)).getOrElse(parametersParam)

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new From(address, tag, _parameters)
  }

  def encodeAsJValue() = FromJsonEncoder.encode(this)

}
