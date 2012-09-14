package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.URI
import org.sisioh.sip.util.Encodable

object GenericURI {
  def apply(uriString: String, schemeParam: Option[String]) = new GenericURI(uriString, schemeParam)
  def unapply(genericUri: GenericURI): Option[(String, String)] = Some(genericUri.uriString, genericUri.scheme)
}

class GenericURI(val uriString: String, schemeParam: Option[String] = None) extends URI with Encodable[GenericURI] {
  private val i = uriString.indexOf(":")
  require(i != -1)
  val scheme = schemeParam.getOrElse(uriString.substring(0, i))

  val isSipURI = isInstanceOf[SipUri]

  def encode = uriString

  def encode(sb: StringBuilder): StringBuilder = sb.append(uriString)

  override def hashCode() = 31 * uriString.## + 31 * scheme.##

  override def equals(obj: Any) = obj match {
    case that: GenericURI =>
      uriString == that.uriString && scheme == that.scheme
    case _ => false
  }

}
