package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.{ParameterNames, FromHeader}
import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl.{DefaultAddressParser, AddressType, DefaultAddress}
import org.sisioh.sip.core.Separators

object FromDecoder extends FromDecoder

class FromDecoder extends SIPDecoder[From] with FromParser {
  def decode(source: String):From = decodeTarget(source, fromWithCrLfOpt)
}

trait FromParser extends ToOrFromParser with DefaultAddressParser {
  lazy val fromWithCrLfOpt: Parser[From] = from <~ opt(CRLF)

  lazy val from: Parser[From] = ("From" | "f") ~> HCOLON ~> ((nameAddrToDefaultAddress | addrSpecToDefaultAddress) ~ rep(SEMI ~> toParam)) ^^ {
    case da ~ toParams =>
      From(da, None, NameValuePairList.fromValues(toParams))
  }

}

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

  object JsonEncoder extends Encoder[From] {
    def encode(model: From, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(
        JField("headerName", JString(model.headerName)) ::
        JField("address", parse(model.address.encodeByJson())) ::
        JField("paramters", parse(model.parameters.encodeByJson())) :: Nil
      )
      builder.append(compact(render(json)))
    }
  }

}

/**
 * Fromヘッダを表す値オブジェクト。
 *
 * @param address
 * @param tag
 * @param parametersParam
 */
class From
(val address: DefaultAddress,
 val tag: Option[String] = None,
 parametersParam: NameValuePairList = NameValuePairList())
  extends ToOrFromHeader with FromHeader {

  type ParametersHeaderType = From

  val headerName = FromHeader.NAME
  val name = headerName

  val parameters = tag.map(t => parametersParam.add(ParameterNames.TAG, t)).getOrElse(parametersParam)

  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  def hasTag: Boolean = hasParameter(ParameterNames.TAG)

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new From(address, tag, _parameters)
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, From.JsonEncoder)

}
