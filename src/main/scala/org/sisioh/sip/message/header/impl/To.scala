package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ToHeader
import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl._
import org.sisioh.sip.core.Separators
import scala.Some
import org.sisioh.sip.message.address.{URI, SipURI}
import org.sisioh.sip.message.address.impl.AddressType
import scala.Some
import scala.Some

object ToDecoder extends ToDecoder

class ToDecoder extends Decoder with ToParser {
  def decode(source: String) = decodeTarget(source, toWithCrLfOpt)
}

trait ToParser extends ToOrFromParser with DefaultAddressParser {

  lazy val toWithCrLfOpt: Parser[To] = to <~ opt(CRLF)

  lazy val to: Parser[To] = ("To" | "t") ~> HCOLON ~> ((nameAddrToDefaultAddress | addrSpecToDefaultAddress) ~ rep(SEMI ~> toParam)) ^^ {
    case da ~ toParams =>
      To(da, None, NameValuePairList.fromValues(toParams))
  }

}

/**
 * [[org.sisioh.sip.message.header.impl.To]]のためのコンパニオンオブジェクト。
 */
object To {

  def apply
  (address: DefaultAddress,
   tag: Option[String] = None,
   parameters: NameValuePairList = NameValuePairList()) = new To(address, tag, parameters)

  def fromFrom(from: From) =
    new To(from.address)

  def unapply(to: To): Option[(DefaultAddress, NameValuePairList)] =
    Some(to.address, to.parameters)

  def decode(source: String) = ToDecoder.decode(source)

  object JsonEncoder extends Encoder[To] {
    def encode(model: To, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("address", parse(model.address.encodeByJson())) ::
        JField("paramters", parse(model.parameters.encodeByJson())) :: Nil)
      builder.append(compact(render(json)))
    }
  }

}

/**
 * Toヘッダを表す値オブジェクト。
 *
 * @param address [[org.sisioh.sip.message.address.impl.DefaultAddress]]
 * @param tag タグ
 * @param parametersParam [[org.sisioh.sip.util.NameValuePairList]]
 */
class To
(val address: DefaultAddress,
 val tag: Option[String] = None,
 parametersParam: NameValuePairList = NameValuePairList())
  extends ToOrFromHeader with ToHeader {

  val name = ToHeader.NAME
  val headerName = ToHeader.NAME

  val parameters = tag.map(t => parametersParam.add("tag", t)).getOrElse(parametersParam)

  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new To(address, tag, _parameters)
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, To.JsonEncoder)

}
