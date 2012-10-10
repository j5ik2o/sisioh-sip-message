package org.sisioh.sip.message.header.impl

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

import org.sisioh.sip.util._
import org.sisioh.sip.message.header._
import org.sisioh.sip.core.Separators
import net.liftweb.json._

object ViaListDecoder extends ViaListDecoder

class ViaListDecoder extends SIPDecoder[ViaList] with ViaListParser {
  def decode(source: String) = decodeTarget(source, VIA)
}

trait ViaListParser extends ParserBase with HostParser {

  lazy val VIA: Parser[ViaList] = ("Via" | "v") ~> HCOLON ~> rep1sep(viaParm, COMMA) ^^ {
    case vias => ViaList(vias)
  }

  lazy val viaParm: Parser[Via] = sentProtocol ~ (LWS ~> sentBy) ~ rep(SEMI ~> viaParams) ^^ {
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

  lazy val sentBy: Parser[HostPort] = host ~ opt(COLON ~> port) ^^ {
    case f ~ s =>
      HostPort(Host(f), s.map(_.toInt))
  }


}

trait ViaJsonFiledNames extends JsonFieldNames {

  val SENT_BY = "sentBy"

  val SENT_PROTOCOL = "sentProtocol"

}

object ViaJsonDecoder extends JsonDecoder[Via] with ViaJsonFiledNames {

  def decode(json: JsonAST.JValue) = {
    requireHeaderName(json, ViaHeader.NAME)
    val sentBy = HostPortJsonDecoder.decode(json \ SENT_BY)
    val sentProtocol = ProtocolJsonDecoder.decode(json \ SENT_PROTOCOL)
    val paramters = NameValuePairListJsonDecoder.decode(json \ PARAMETERS)
    Via(sentBy, sentProtocol, paramters)
  }

}

object ViaJsonEncoder extends JsonEncoder[Via] with ViaJsonFiledNames {

  def encode(model: Via) = {
    JObject(
      getHeaderNameAsJValue(model) ::
      JField(SENT_BY, model.sentBy.encodeAsJValue()) ::
      JField(SENT_PROTOCOL, model.sentProtocol.encodeAsJValue()) ::
      JField(PARAMETERS, model.parameters.encodeAsJValue()) :: Nil
    )
  }

}

object ViaListJsonDecoder extends SIPHeaderListJsonDecoder[ViaList, Via]{
  protected def createInstance(sipHeaders: List[Via]) =
    ViaList(sipHeaders)
}


object Via {


}

case class Via
(sentBy: HostPort,
 sentProtocol: Protocol,
 parameters: NameValuePairList = NameValuePairList())
  extends ParametersHeader with ViaHeader with ViaHeaderExt {

  type ParametersHeaderType = Via

  val headerName = ViaHeader.NAME
  val name = headerName

  val duplicates = DuplicateNameValueList()

  protected def createParametersHeader(duplicates: DuplicateNameValueList, _parameters: NameValuePairList): Via = {
    Via(sentBy, sentProtocol, _parameters)
  }

  val host = sentBy.host.hostName.get
  val port = sentBy.port
  val transport = sentProtocol.transport
  val protocol = sentProtocol.protocolName
  val protocolVersion = sentProtocol.protocolVersion
  val ttl = parameters.getValueAsInt(ParameterNames.TTL)
  val maddr = parameters.getValueAsString(ParameterNames.MADDR)
  val received = parameters.getValueAsString(ParameterNames.RECEIVED)
  val branch = parameters.getValueAsString(ParameterNames.BRANCH)
  val rPort = parameters.getValueAsInt(ParameterNames.RPORT)

  def withBranch(branch: String): Via = withParameter(ParameterNames.BRANCH, branch)

  def withTtl(ttl: Int): Via = withParameter(ParameterNames.TTL, ttl)

  def withMAddr(maddr: String): Via = withParameter(ParameterNames.MADDR, maddr)

  def withReceived(received: String): Via = withParameter(ParameterNames.RECEIVED, received)

  def withRPort(rPort: Int): Via = withParameter(ParameterNames.RPORT, rPort)

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

  def encodeAsJValue() = ViaJsonEncoder.encode(this)
}

case class ViaList(override val headers: List[Via] = List.empty)
  extends SIPHeaderList[ViaList, Via](classOf[Via], ViaHeader.NAME, false, headers) {

  val name = headerName

  protected def createInstance(_headers: List[Via]) = ViaList(_headers)

  override def encodeBody(builder: StringBuilder): StringBuilder =
    encodeBody(builder, Separators.COMMA)

}



