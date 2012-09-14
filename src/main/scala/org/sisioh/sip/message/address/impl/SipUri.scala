package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{NetObject, SipURI}
import org.sisioh.sip.util.{Encodable, Encoder, NameValuePairList}
import org.sisioh.sip.core.Separators

object SipUri {

  implicit object DefaultSipUriEncoder extends Encoder[SipUri] {
    def encode(model: SipUri, builder: StringBuilder) = {
      builder.append(model.scheme)
    }
  }

  implicit object StringEncoder extends Encoder[String] {
    def encode(model: String, builder: StringBuilder) = builder.append(model)
  }

}

class SipUri
(uriString: String,
 schemeParam: Option[String],
 val userName: Option[String],
 val userPassword: Option[String],
 val host: String,
 val port: Option[Int],
 val authority: Option[Authority],
 private val uriParms: NameValuePairList[Any] = NameValuePairList(),
 private val qheaders: NameValuePairList[Any] = NameValuePairList("&"),
 private val encoder: Encoder[Any])
  extends GenericURI(uriString, schemeParam) with SipURI {

  val isSecure: Boolean = scheme.equalsIgnoreCase(NetObject.SIPS)
  override val isSipURI: Boolean = true

  def setHeader(name: String, value: String) = {
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms,
      qheaders.add(name, value),
      encoder)
  }

  def setParamter(name: String, value: String) = {
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms.add(name, value),
      qheaders,
      encoder)
  }

  def setMethod(method: String) = setParamter("method", method)

  def setMAddr(maddr: String) = setParamter("maddr", maddr)

  def setTransport(transport: String) = {
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms.add(NetObject.TRANSPORT, transport.toLowerCase),
      qheaders,
      encoder)
  }

  def setTtl(ttl: Int) = {
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms.add(NetObject.TTL, ttl),
      qheaders,
      encoder)
  }

  def setSecure(secure: Boolean) = {
    new SipUri(uriString,
      Some(if (secure) NetObject.SIPS else NetObject.SIP),
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms,
      qheaders,
      encoder)
  }

  def clearUriParms =
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      NameValuePairList(),
      qheaders,
      encoder)

  def clearQheaders =
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms,
      NameValuePairList("&"),
      encoder)


  private def userWithHostWithPort =
    authority.map {
      a =>
        (a.userInfo.map(_.name),
          a.host.map(_.encode),
          a.port)
    }.get

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

  def getParameter(name: String): Option[String] = {
    uriParms.getValue(name).map {
      e =>
        encoder.encode(e, new StringBuilder).result()
    }
  }

  def removeUserType = {
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms.remove(NetObject.USER),
      qheaders,
      encoder)
  }
  def remvoeHeaders = {
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms,
      NameValuePairList("&"),
      encoder)
  }
  def removeHeader(name: String) = {
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms,
      qheaders.remove(name),
      encoder)
  }

  def removeParameter(name: String) = {
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority,
      uriParms.remove(name),
      qheaders,
      encoder)
  }

  def removePort = {
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      host,
      port,
      authority.map(_.removePort),
      uriParms,
      qheaders,
      encoder)
  }

  def getHeader(name: String) = qheaders.getValue(name).map {
    e =>
      encoder.encode(e, new StringBuilder).result()
  }

  def getHeaderNames = qheaders.names


}
