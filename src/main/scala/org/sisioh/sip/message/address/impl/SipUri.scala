package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{NetObject, SipURI}
import org.sisioh.sip.util._
import org.sisioh.sip.core.{GenericObject, Separators}
import scala.Some

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

  def encode(model: SipUri):String = {
    val builder = new StringBuilder
    builder.append(model.scheme).append(Separators.COLON)
    model.authority.encode(builder)
    if (!model.uriParms.isEmpty) {
      builder.append(Separators.SEMICOLON)
      model.uriParms.encode(builder)
    }
    if (!model.qheaders.isEmpty) {
      builder.append(Separators.QUESTION)
      model.qheaders.encode(builder)
    }
    builder.result()
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
  override val userPassword = authority.userInfo.map(_.password.get)
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

  def setTransport(transport: String) = withParamter(NetObject.TRANSPORT, transport.toLowerCase)

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

  def encode(builder: StringBuilder) = {
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
