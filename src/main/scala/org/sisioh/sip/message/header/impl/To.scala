package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ToHeader
import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl._
import org.sisioh.sip.core.Separators
import scala.Some
import org.sisioh.sip.message.address.{URI, SipURI}
import org.sisioh.sip.message.address.impl.AddressType
import scala.Some

object ToDecoder {
  def apply() = new ToDecoder
}

class ToDecoder extends Decoder with ToParser {
  def decode(source: String) = decodeTarget(source, toWithCrLfOpt)
}

trait ToParser extends ParserBase with DefaultAddressParser {
  lazy val toWithCrLfOpt: Parser[To] = to <~ opt(CRLF)

  lazy val to: Parser[To] = ("To" | "t") ~> HCOLON ~> ((nameAddrToDefaultAddress | addrSpecToDefaultAddress) ~ rep(SEMI ~> toParam)) ^^ {
    case da ~ toParams =>
      To(da, None, NameValuePairList.fromValues(toParams))
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

object To {
  def apply
  (address: DefaultAddress,
   tag: Option[String] = None,
   parameters: NameValuePairList = NameValuePairList()) = new To(address, tag, parameters)

  def fromFrom(from: From) =
    new To(from.address)

  def unapply(to: To): Option[(DefaultAddress, NameValuePairList)] =
    Some(to.address, to.parameters)

  object JsonEncoder extends Encoder[To] {
    def encode(model: To, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("address", parse(model.address.encodeByJson())) ::
        JField("paramters", parse(model.parameters.encodeByJson())) :: Nil)
      builder.append(compact(render(json)))
    }
  }

}

class To
(val address: DefaultAddress,
 val tag: Option[String] = None,
 parametersParam: NameValuePairList = NameValuePairList())
  extends AddressParametersHeader with ToHeader {

  val parameters = tag.map(t => parametersParam.add("tag", t)).getOrElse(parametersParam)

  val headerName = ToHeader.NAME
  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new To(address, tag, _parameters)
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
    case that: To =>
//      println("address",address, that.address, (address == that.address))
//      println("parameters",parameters, that.parameters, (parameters == that.parameters))
//      println("headerName",headerName, that.headerName, (headerName == that.headerName))
//      println("duplicates",duplicates, that.duplicates, (duplicates == that.duplicates))
      address == that.address &&
        parameters == that.parameters &&
        headerName == that.headerName &&
        duplicates == that.duplicates
    case _ => false
  }

  override def toString = encode()

  def encodeByJson(builder: StringBuilder) = encode(builder, To.JsonEncoder)

  val name = ToHeader.NAME
}
