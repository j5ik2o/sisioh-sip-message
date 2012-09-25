package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util._
import org.sisioh.sip.message.header.{ViaHeaderExt, ViaHeader, Protocol}
import scala.Some
import org.sisioh.sip.message.address.impl.ParameterNames
import org.sisioh.sip.core.Separators

trait ViaParser extends ParserBase with HostParser {
  lazy val VIA = ("Via" | "v") ~> HCOLON ~> rep1sep(viaParm, COMMA)

  lazy val viaParm = sentProtocol ~ (LWS ~> sentBy) ~ rep(SEMI ~> viaParams) ^^ {
    case sp ~ sb ~ params =>
      Via(sb, sp, NameValuePairList.fromValues(params))
  }
  lazy val viaParams = viaTtl | viaMaddr | viaReceived | viaBranch | viaExtension
  lazy val viaTtl: Parser[NameValuePair] = "ttl" ~> EQUAL ~> ttl ^^ {
    e =>
      NameValuePair(Some(ParameterNames.TTL), Some(e))
  }
  lazy val viaMaddr: Parser[NameValuePair] = "maddr" ~> EQUAL ~> host ^^ {
    e =>
      NameValuePair(Some(ParameterNames.MADDR), Some(e))
  }
  lazy val viaReceived: Parser[NameValuePair] = "received" ~> EQUAL ~> (IPv4address | IPv6address) ^^ {
    e =>
      NameValuePair(Some(ParameterNames.MADDR), Some(e))
  }
  lazy val viaBranch = "branch" ~> EQUAL ~> token ^^ {
    e =>
      NameValuePair(Some(ParameterNames.BRANCH), Some(e))
  }
  lazy val viaExtension = genericParam

  lazy val sentProtocol: Parser[Protocol] = protocolName ~ (SLASH ~> protocolVersion) ~ (SLASH ~> transport) ^^ {
    case pn ~ pv ~ ts =>
      Protocol(pn, pv, ts)
  }

  lazy val protocolName = "SIP" | token
  lazy val protocolVersion = token
  lazy val transport = "UDP" | "TCP" | "TLS" | "SCTP" | otherTransport
  lazy val otherTransport = token

  lazy val sentBy: Parser[HostPort] = host ~ opt(COLON ~> port) ^^ {
    case f ~ s =>
      HostPort(Host(f), s.map(_.toInt))
  }

  lazy val ttl: Parser[Int] = DIGIT ~ opt(DIGIT ~ opt(DIGIT) ^^ {
    case f ~ s =>
      List(Some(f), s).mkString
  }) ^^ {
    case f ~ s =>
      List(Some(f), s).mkString.toInt
  }
}

object Via {

  object JsonEncoder extends Encoder[Via] {
    def encode(model: Via, builder: StringBuilder) = {
      null
    }

  }

}

case class Via
(sentBy: HostPort,
 sentProtocol: Protocol,
 parameters: NameValuePairList = NameValuePairList())
  extends ParametersHeader with ViaHeader with ViaHeaderExt {

  val headerName = ViaHeader.NAME

  val duplicates = DuplicateNameValueList()

  protected def createParametersHeader(duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    Via(sentBy, sentProtocol, _parameters)
  }

  val host = sentBy.host.hostName.get
  val port = sentBy.port
  val transport = sentProtocol.transport
  val protocol = sentProtocol.protocolName
  val ttl = parameters.getValueAsInt(ParameterNames.TTL)
  val maddr = parameters.getValueAsString(ParameterNames.MADDR)
  val received = parameters.getValueAsString(ParameterNames.RECEIVED)
  val branch = parameters.getValueAsString(ParameterNames.BRANCH)
  val rPort = parameters.getValueAsInt(ParameterNames.RPORT)

  def encodeByJson(builder: StringBuilder) = encode(builder, Via.JsonEncoder)

  def encodeBody(builder: StringBuilder) = {
    sentProtocol.encode(builder)
    builder.append(Separators.SP)
    sentBy.encode(builder)
    if (!parameters.isEmpty) {
      builder.append(Separators.SEMICOLON)
      parameters.encode(builder)
    }
    //    if (comment != null) {
    //      builder.append(Separators.SP).append(Separators.LPAREN).append(comment).append(Separators.RPAREN);
    //    }
    builder
  }

  val sentByField = sentBy.toString
  val sentByProtocolField = sentProtocol.toString
}
