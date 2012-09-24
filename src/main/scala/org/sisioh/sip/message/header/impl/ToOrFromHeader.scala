package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util.{NameValuePairList, HostParser, NameValuePair, ParserBase}
import org.sisioh.sip.message.address.impl.AddressType
import org.sisioh.sip.core.Separators

trait ToOrFromParser extends ParserBase with HostParser {

  lazy val toParam = tagParam | genericParam

  lazy val tagParam: Parser[NameValuePair] = "tag" ~ (EQUAL ~> token) ^^ {
    case n ~ v => NameValuePair(Some(n), Some(v))
  }

  lazy val genericParam: Parser[NameValuePair] = token ~ opt(EQUAL ~> genValue) ^^ {
    case n ~ v => NameValuePair(Some(n), Some(v))
  }

  lazy val genValue = token | host | quotedString

}

trait ToOrFromHeader extends AddressParametersHeader {

  val tag: Option[String]
  val parameters: NameValuePairList

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
    case that: ToOrFromHeader =>
      address == that.address &&
        parameters == that.parameters &&
        headerName == that.headerName &&
        duplicates == that.duplicates
    case _ => false
  }

  override def toString = encode()

}
