package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{NetObject, SipURI}
import org.sisioh.sip.util._
import org.sisioh.sip.core.{GenericObject, Separators}

object SipUriDecoder {
  def apply() = new SipUriDecoder
}

class SipUriDecoder extends Decoder[SipUri] with SipUriParser {
  def decode(source: String) = decodeTarget(source, sipuri)
}

trait SipUriParser extends ParserBase with UserInfoParser with HostPortParser {

  def sipuri: Parser[SipUri] = "sip:" ~> opt(userInfo) ~ hostPort ~ uriParams ~ opt(headers) ^^ {
    case userInfoOpt ~ hostPort ~ uriParams ~ headersOpt =>
      SipUri.fromUserInfoWithHostPort(userInfoOpt, Some(hostPort))
  }


  def headers: Parser[NameValuePairList] = "?" ~> rep1sep(header, "&") ^^ {
    case hlist =>
      NameValuePairList.fromValues(hlist)
  }

  def header: Parser[NameValuePair] = hname ~ "=" ~ hvalue ^^ {
    case n ~ _ ~ v =>
      NameValuePair(Some(n), Some(v))
  }

  def hname = rep1(hnvUnreserved | unreserved | escaped) ^^ {
    _.mkString
  }

  def hnvUnreserved = "[" | "]" | "/" | "?" | ":" | "+" | "$"

  def hvalue = rep(hnvUnreserved | unreserved | escaped) ^^ {
    _.mkString
  }

  def uriParams: Parser[NameValuePairList] = rep(";" ~> uriParameter) ^^ {
    case values =>
      NameValuePairList.fromValues(values)
  }

  lazy val uriParameter: Parser[NameValuePair] = transportParam | userParam | methodParam | ttlParam | maddrParam | lrParam | otherParam

  lazy val transportParam: Parser[NameValuePair] = "transport=" ~> ("udp" | "tcp" | "sctp" | "tls" | otherTransport) ^^ {
    transport =>
      NameValuePair(Some(NetObject.TRANSPORT), Some(transport))
  }

  lazy val otherTransport: Parser[String] = token

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

  lazy val ttl = repN(3, DIGIT) ^^ {
    _.mkString.toInt
  }

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

  lazy val pname = rep1(paramchar) ^^ {
    _.mkString
  }
  lazy val pvalue = rep1(paramchar) ^^ {
    _.mkString
  }

  lazy val paramchar = paramUnreserved | unreserved | escaped

  lazy val paramUnreserved = "[" | "]" | "/" | ":" | "&" | "+" | "$"
}

object SipUri {

  def apply(authority: Authority, scheme: String = NetObject.SIP): SipUri =
    new SipUri(authority, scheme)

  def fromUserInfoWithHostPort
  (userInfo: Option[UserInfo],
   hostPort: Option[HostPort],
   scheme: String = NetObject.SIP): SipUri = {
    val authority = Authority(hostPort, userInfo)
    apply(authority, scheme)
  }

  def fromUserWithPasswordAndHostWithPort
  (user: Option[String],
   password: Option[String],
   host: Option[String],
   port: Option[Int],
   scheme: String = NetObject.SIP): SipUri = {
    val userInfo = (user, password) match {
      case (Some(u), p) => Some(UserInfo(u, p))
      case _ => None
    }
    val hostPort = (host, port) match {
      case (Some(h), p) => Some(HostPort(Host(h), port))
      case _ => None
    }
    fromUserInfoWithHostPort(userInfo, hostPort, scheme)
  }

  class JsonEncoder extends Encoder[SipUri] {
    def encode(model: SipUri, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("scheme", JString(model.scheme)) ::
        JField("authority", parse(model.authority.encode())) ::
        JField("uriParams", parse(model.uriParms.encode())) ::
        JField("qheaders", parse(model.qheaders.encode())) :: Nil)
      builder.append(compact(render(json)))
    }
  }

}

class SipUri
(val authority: Authority,
 override val scheme: String = NetObject.SIP,
 private val uriParms: NameValuePairList = NameValuePairList(),
 private val qheaders: NameValuePairList = NameValuePairList("&"))
  extends GenericURI with SipURI with GenericObject {

  override val userName = authority.userInfo.map(_.name)
  override val userPassword = authority.userInfo.flatMap(_.password)
  override val host = authority.host.get.hostName.get
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

}
