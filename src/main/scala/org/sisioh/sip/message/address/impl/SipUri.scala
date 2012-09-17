package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{NetObject, SipURI}
import org.sisioh.sip.util.{Host, Encodable, Encoder, NameValuePairList}
import org.sisioh.sip.core.{GenericObject, Separators}

object SipUri {

  def apply(authority: Authority, scheme: String = NetObject.SIP) = new SipUri(authority, scheme)

  implicit object JsonEncoder extends Encoder[SipUri] {
    def encode(model: SipUri, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("scheme", JString(model.scheme)) ::
        JField("authority", parse(model.authority.encode)) ::
        JField("uriParams", parse(model.uriParms.encode)) ::
        JField("qheaders", parse(model.qheaders.encode)) :: Nil)
      builder.append(compact(render(json)))
    }
  }

}

class SipUri
(val authority: Authority,
 val scheme: String = NetObject.SIP,
 private val uriParms: NameValuePairList = NameValuePairList(),
 private val qheaders: NameValuePairList = NameValuePairList("&"))
  extends SipURI with GenericObject[SipUri] {

  override val userName = authority.userInfo.map(_.name)
  override val userPassword = authority.userInfo.map(_.password.get)
  override val host = authority.host.get.hostName.get
  override val port = authority.port
  override val isSecure: Boolean = scheme.equalsIgnoreCase(NetObject.SIPS)
  override val isSipURI: Boolean = true

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
}
