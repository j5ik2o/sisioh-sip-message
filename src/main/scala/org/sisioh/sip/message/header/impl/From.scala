package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.FromHeader
import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl.{DefaultAddressParser, AddressType, DefaultAddress}
import org.sisioh.sip.core.Separators

object FromDecoder {
  def apply() = new FromDecoder
}

class FromDecoder extends Decoder with FromParser {
  def decode(source: String) = decodeTarget(source, fromWithCrLfOpt)
}

trait FromParser extends ParserBase with DefaultAddressParser {
  lazy val fromWithCrLfOpt: Parser[From] = from <~ opt(CRLF)

  lazy val from: Parser[From] = ("From" | "f") ~> HCOLON ~> ((nameAddrToDefaultAddress | addrSpecToDefaultAddress) ~ rep(SEMI ~> toParam)) ^^ {
    case da ~ toParams =>
      From(da, None, NameValuePairList.fromValues(toParams))
  }

  lazy val toParam = tagParam | genericParam

  lazy val tagParam: Parser[NameValuePair] = "tag" ~ (EQUAL ~> token) ^^ {
    case n ~ v => NameValuePair(Some(n), Some(v))
  }

  lazy val genericParam: Parser[NameValuePair] = token ~ opt(EQUAL ~> genValue) ^^ {
    case n ~ v => NameValuePair(Some(n), Some(v))
  }

  lazy val genValue = token | host | quotedString

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

  object JsonEncoder extends Encoder[To] {
    def encode(model: To, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("address", parse(model.address.encodeByJson())) ::
        JField("paramters", parse(model.parameters.encodeByJson())) :: Nil)
      builder.append(compact(render(json)))
    }
  }

}

class From
(val address: DefaultAddress,
 val tag: Option[String] = None,
 parametersParam: NameValuePairList = NameValuePairList())
  extends AddressParametersHeader with FromHeader {
  val parameters = tag.map(t => parametersParam.add("tag", t)).getOrElse(parametersParam)

  val headerName = FromHeader.NAME
  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new From(address, tag, _parameters)
  }

  def encodeBody(builder: StringBuilder) = {
    if (address.addressType == AddressType.ADDRESS) {
      builder.append(Separators.LESS_THAN)
    }
    address.encode(builder)
    if (address.addressType == AddressType.ADDRESS) {
      builder.append(Separators.GREATER_THAN)
    }
    if (!parameters.isEmpty) {
      builder.append(Separators.SEMICOLON)
      parameters.encode(builder)
    }
    builder
  }

  override def hashCode() = 31 * address.## + 31 * parameters.## + 31 * headerName.## + 31 * duplicates.##

  override def equals(obj: Any) = obj match {
    case that: From =>
      address == that.address &&
        parameters == that.parameters &&
        headerName == that.headerName &&
        duplicates == that.duplicates
    case _ => false
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, From.JsonEncoder)

  val name = FromHeader.NAME
}
