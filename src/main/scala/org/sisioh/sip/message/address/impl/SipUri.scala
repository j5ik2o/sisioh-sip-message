package org.sisioh.sip.message.address.impl

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


import org.sisioh.sip.message.address.{NetObject, SipURI}
import org.sisioh.sip.util._
import org.sisioh.sip.core.{GenericObject, Separators}
import net.liftweb.json._
import net.liftweb.json

object SipUriDecoder extends SipUriDecoder

class SipUriDecoder extends SIPDecoder[SipUri] with SipUriParser {
  def decode(source: String) = decodeTarget(source, uri)

  lazy val uri = SIP_URI | SIPS_URI
}

trait SipUriParser extends ParserBase with DefaultGenericURIParser with UserInfoParser with HostPortParser {

  lazy val SIP_URI: Parser[SipUri] = "sip:" ~> opt(userInfoWithAt) ~ hostPort ~ uriParams ~ opt(headers) ^^ {
    case userInfoOpt ~ hostPort ~ uriParams ~ headersOpt =>
      SipUri.fromUserInfoAndHostPort(userInfoOpt, Some(hostPort), NetObject.SIP)
  }

  lazy val SIPS_URI: Parser[SipUri] = "sips:" ~> opt(userInfoWithAt) ~ hostPort ~ uriParams ~ opt(headers) ^^ {
    case userInfoOpt ~ hostPort ~ uriParams ~ headersOpt =>
      SipUri.fromUserInfoAndHostPort(userInfoOpt, Some(hostPort), NetObject.SIPS)
  }

  lazy val headers: Parser[NameValuePairList] = '?' ~> rep1sep(header, '&') ^^ {
    case hlist =>
      NameValuePairList.fromValues(hlist)
  }

  lazy val header: Parser[NameValuePair] = hname ~ ('=' ~> hvalue) ^^ {
    case n ~ v =>
      NameValuePair(Some(n), Some(v))
  }

  lazy val hname: Parser[String] = rep1(hnvUnreserved | unreserved | escaped) ^^ {
    _.mkString
  }

  lazy val hnvUnreserved: Parser[Char] = elem('[') | ']' | '/' | '?' | ':' | '+' | '$'

  lazy val hvalue: Parser[String] = rep(hnvUnreserved | unreserved | escaped) ^^ {
    _.mkString
  }

  lazy val uriParams: Parser[NameValuePairList] = rep(";" ~> uriParameter) ^^ {
    case values =>
      NameValuePairList.fromValues(values)
  }

  lazy val uriParameter: Parser[NameValuePair] = transportParam | userParam | methodParam | ttlParam | maddrParam | lrParam | otherParam

  lazy val transportParam: Parser[NameValuePair] = "transport=" ~> ("udp" | "tcp" | "sctp" | "tls" | otherTransport) ^^ {
    transport =>
      NameValuePair(Some(NetObject.TRANSPORT), Some(transport))
  }

  lazy val userParam: Parser[NameValuePair] = "user=" ~> ("phone" | "ip" | otherUser) ^^ {
    user =>
      NameValuePair(Some(NetObject.USER), Some(user))
  }

  lazy val otherUser: Parser[String] = token

  lazy val methodParam: Parser[NameValuePair] = "method=" ~> Method ^^ {
    method =>
      NameValuePair(Some(NetObject.METHOD), Some(method))
  }

  lazy val ttlParam: Parser[NameValuePair] = "ttl=" ~> ttl ^^ {
    ttl =>
      NameValuePair(Some(NetObject.TTL), Some(ttl))
  }

//  lazy val ttl: Parser[Int] = repN(3, DIGIT) ^^ {
//    _.mkString.toInt
//  }

  lazy val maddrParam: Parser[NameValuePair] = "maddr=" ~> host ^^ {
    case host =>
      NameValuePair(Some(NetObject.MADDR), Some(host.toString))
  }

  lazy val lrParam: Parser[NameValuePair] = "lr" ^^ {
    _ => NameValuePair(Some(NetObject.LR), Some(true))
  }

  lazy val otherParam: Parser[NameValuePair] = pname ~ opt("=" ~> pvalue) ^^ {
    case pn ~ pv =>
      NameValuePair(Some(pn), Some(pv))
  }

  lazy val pname: Parser[String] = rep1(paramchar) ^^ {
    _.mkString
  }
  lazy val pvalue: Parser[String] = rep1(paramchar) ^^ {
    _.mkString
  }

  lazy val paramchar: Parser[Char] = paramUnreserved | unreserved | escaped

  lazy val paramUnreserved: Parser[Char] = elem('[') | ']' | '/' | ':' | '&' | '+' | '$'
}

object SipUriJsonDecoder extends JsonDecoder[SipUri] {
  def decode(json: JsonAST.JValue) = {
    val JString(scheme) = json \ "scheme"
    val authority = AuthorityJsonDecoder.decode(json \ "authority")
    val uriParams = NameValuePairListJsonDecoder.decode(json \ "uriParams")
    val qheaders = NameValuePairListJsonDecoder.decode(json \ "qheaders")
    SipUri(authority, scheme, uriParams, qheaders)
  }
}

object SipUriJsonEncoder extends JsonEncoder[SipUri] {

  def encode(model: SipUri) = {
    JObject(JField("scheme", JString(model.scheme)) ::
      JField("authority", parse(model.authority.encodeByJson())) ::
      JField("uriParams", parse(model.uriParms.encodeByJson())) ::
      JField("qheaders", parse(model.qheaders.encodeByJson())) :: Nil)
  }

}

object SipUri {

  def apply
  (authority: Authority,
   scheme: String = NetObject.SIP,
   uriParms: NameValuePairList = NameValuePairList(),
   qheaders: NameValuePairList = NameValuePairList("&")) =
    new SipUri(authority, scheme, uriParms, qheaders)

  def decode(source: String) = SipUriDecoder.decode(source)

  def fromUserInfoAndHostPort
  (userInfo: Option[UserInfo],
   hostPort: Option[HostPort],
   scheme: String = NetObject.SIP): SipUri = {
    val authority = Authority(hostPort, userInfo)
    apply(authority, scheme)
  }

  def fromUserAndHost
  (user: Option[String],
   password: Option[String],
   host: String,
   port: Option[Int],
   scheme: String = NetObject.SIP): SipUri = {
    val userInfo = (user, password) match {
      case (Some(u), p) => Some(UserInfo(u, p))
      case _ => None
    }
    val hostPort = (host, port) match {
      case (h, p) => Some(HostPort(Host(h), port))
      case _ => None
    }
    fromUserInfoAndHostPort(userInfo, hostPort, scheme)
  }


}

class SipUri
(val authority: Authority,
 override val scheme: String = NetObject.SIP,
 val uriParms: NameValuePairList = NameValuePairList(),
 val qheaders: NameValuePairList = NameValuePairList("&"))
  extends GenericURI with SipURI with GenericObject {

  override val userName = authority.userInfo.map(_.name)
  override val userPassword = authority.userInfo.flatMap(_.password)
  override val host = {
    //println("host",authority.host)
    val host = authority.host.get
    host.hostName.get
  }
  override val port = authority.port
  override val isSecure: Boolean = scheme.equalsIgnoreCase(NetObject.SIPS)
  override val isSipURI: Boolean = true

  val uriString = toString

  def withHeader(name: String, value: Any) = {
    new SipUri(
      authority,
      scheme,
      uriParms,
      qheaders.add(name, value))
  }

  def withParamter(name: String, value: Any) = {
    new SipUri(
      authority,
      scheme,
      uriParms.add(name, value),
      qheaders)
  }

  def withUser(user: String) = withParamter(NetObject.USER, user)

  override def getUser = getParameter(NetObject.USER).map(_.asInstanceOf[String])

  def withMethod(method: String) = withParamter(NetObject.METHOD, method)

  override def getMethod = getParameter(NetObject.METHOD).map(_.asInstanceOf[String])

  def withMAddr(maddr: String) = withParamter(NetObject.MADDR, maddr)

  override def getMAddr = getParameter(NetObject.MADDR).map(_.asInstanceOf[String])

  def withTransport(transport: String) = withParamter(NetObject.TRANSPORT, transport.toLowerCase)

  override def getTransport = getParameter(NetObject.TRANSPORT).map(_.asInstanceOf[String])

  def withTtl(ttl: Int) = withParamter(NetObject.TTL, ttl)

  override def getTtl = getParameter(NetObject.TTL).map(_.asInstanceOf[Int])

  def withHasLr(flag: Boolean) = withParamter(NetObject.LR, flag)

  override def hasLr = getParameter(NetObject.LR).map(_.asInstanceOf[Boolean])

  def setSecure(secure: Boolean) = {
    new SipUri(
      authority,
      if (secure) NetObject.SIPS else NetObject.SIP,
      uriParms,
      qheaders)
  }

  def clearUriParms =
    new SipUri(
      authority,
      scheme,
      NameValuePairList(),
      qheaders)

  def clearQheaders =
    new SipUri(
      authority,
      scheme,
      uriParms,
      NameValuePairList())


  private def userWithHostWithPort =
    (authority.userInfo.map(_.name),
      authority.host.map(_.encode()),
      authority.port)

  def getUserAtHost = {
    "%s%s".format(
      userWithHostWithPort._1.map(_ + Separators.AT).getOrElse(""),
      userWithHostWithPort._2.get
    )
  }

  def getUserAtHostPort = {
    "%s%s%s".format(
      userWithHostWithPort._1.map(_ + Separators.AT).getOrElse(""),
      userWithHostWithPort._2.get,
      userWithHostWithPort._3.map(Separators.COLON + _).getOrElse("")
    )
  }

  def parameterNames = uriParms.names

  def getParameter(name: String): Option[Any] = uriParms.getValue(name)

  def removeUserType = {
    new SipUri(
      authority,
      scheme,
      uriParms.remove(NetObject.USER),
      qheaders)
  }

  def remvoeHeaders = {
    new SipUri(
      authority,
      scheme,
      uriParms,
      NameValuePairList())
  }

  def removeHeader(name: String) = {
    new SipUri(
      authority,
      scheme,
      uriParms,
      qheaders.remove(name))
  }

  def removeParameter(name: String) = {
    new SipUri(
      authority,
      scheme,
      uriParms.remove(name),
      qheaders)
  }

  def removePort = {
    new SipUri(
      authority.removePort,
      scheme,
      uriParms,
      qheaders)
  }

  def getHeader(name: String) = qheaders.getValue(name)

  def getHeaderNames = qheaders.names

  override def encode(builder: StringBuilder) = {
    builder.append(scheme).append(Separators.COLON)
    authority.encode(builder)
    if (!uriParms.isEmpty) {
      builder.append(Separators.SEMICOLON)
      uriParms.encode(builder)
    }
    if (!qheaders.isEmpty) {
      builder.append(Separators.QUESTION)
      qheaders.encode(builder)
    }
    builder
  }

  override def hashCode() =
    31 * authority.## + 31 * scheme.## + 31 * uriParms.## + 31 * qheaders.##

  override def equals(obj: Any) = obj match {
    case that: SipUri =>
      authority == that.authority &&
        scheme == that.scheme &&
        uriParms == that.uriParms &&
        qheaders == that.qheaders
    case _ => false
  }

  override def toString = encode()

  def encodeAsJValue() = SipUriJsonEncoder.encode(this)
}
