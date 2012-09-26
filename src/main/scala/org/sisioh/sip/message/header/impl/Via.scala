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
import org.sisioh.sip.message.header.{impl, ViaHeaderExt, Protocol, ViaHeader}
import scala.Some
import org.sisioh.sip.message.address.impl.{TelephoneNumber, ParameterNames}
import org.sisioh.sip.core.Separators
import collection.generic.CanBuildFrom
import collection.{LinearSeqOptimized, immutable, mutable}
import collection.immutable.LinearSeq
import collection.mutable.Builder

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
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val json = ("sentBy" -> parse(model.sentBy.encodeByJson())) ~
        ("sentProtocol" -> parse(model.sentProtocol.encodeByJson())) ~
        ("parameters" -> parse(model.parameters.encodeByJson()))
      builder.append(compact(render(json)))
    }

  }

}

case class Via
(sentBy: HostPort,
 sentProtocol: Protocol,
 parameters: NameValuePairList = NameValuePairList())
  extends ParametersHeader with ViaHeader with ViaHeaderExt {

  type ParametersHeaderType = Via

  val headerName = ViaHeader.NAME

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

case class ViaList(headers: List[Via] = List.empty)
  extends SIPHeaderList[ViaList, Via](classOf[Via], ViaHeader.NAME, false, headers) {

  protected def createInstance(_headers: List[Via]) = ViaList(_headers)

}



