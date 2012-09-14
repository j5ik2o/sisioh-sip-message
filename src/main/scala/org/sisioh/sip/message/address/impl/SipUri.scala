package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{NetObject, SipURI}
import org.sisioh.sip.util.{Encoder, NameValuePairList}
import org.sisioh.sip.core.Separators

object SipUri {
  implicit object DefaultSipUriEncoder extends Encoder[SipUri]{
    def encode(model: SipUri, builder: StringBuilder) = {
      builder.append(model.scheme)
    }
  }
}

class SipUri
(uriString: String,
 schemeParam: Option[String],
 val userName: Option[String],
 val userPassword: Option[String],
 val isSecure: Boolean,
 val host: String,
 val port: Option[Int],
 val authority: Option[Authority],
 private val uriParms: NameValuePairList[String] = NameValuePairList(),
 private val qheaders: NameValuePairList[String] = NameValuePairList("&"))
  extends GenericURI(uriString, schemeParam) with SipURI {

  def clearUriParms: SipUri =
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      isSecure,
      host,
      port,
      authority,
      NameValuePairList(),
      qheaders)

  def clearQheaders: SipUri =
    new SipUri(uriString,
      schemeParam,
      userName,
      userPassword,
      isSecure,
      host,
      port,
      authority,
      uriParms,
      NameValuePairList("&"))


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

  def parameterNames = null

  def getParameter(name: String) = null

  def removeParameter(name: String) = null

  def removePort = null

  def getHeader(name: String) = null

  def getHeaderNames = null

}
