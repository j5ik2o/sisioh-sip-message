package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.URI
import org.sisioh.sip.util._
import org.sisioh.sip.core.GenericObject
import util.parsing.combinator.RegexParsers
import org.sisioh.sip.util.ParseException
import scala.Some

/**
 * [[org.sisioh.sip.message.address.impl.DefaultGenericURIDecoder]]のコンパニオンオブジェクト。
 */
object DefaultGenericURIDecoder {
  /**
   * ファクトリメソッド。
   *
   * @return [[org.sisioh.sip.message.address.impl.DefaultGenericURIDecoder]]
   */
  def apply() = new DefaultGenericURIDecoder
}

/**
 * [[org.sisioh.sip.message.address.impl.GenericURI]]のための[[org.sisioh.sip.util.Decoder]]
 */
class DefaultGenericURIDecoder extends Decoder[GenericURI] with DefaultGenericURIParser {

  def decode(source: String): GenericURI = decodeTarget(source, genericURI)

}

trait DefaultGenericURIParser extends RegexParsers {

  def genericURI: Parser[GenericURI] = URI ^^ {
    uri => DefaultGenericURI(uri)
  }

  lazy val URI = """.+""".r
}


object DefaultGenericURI {

  def apply(uriString: String, schemeParam: Option[String] = None) = new DefaultGenericURI(uriString, schemeParam)

  def unapply(genericUri: GenericURI): Option[(String, String)] = Some(genericUri.uriString, genericUri.scheme)

  def decode(source: String) = DefaultGenericURIDecoder().decode(source)

}

trait GenericURI extends URI with GenericObject {
  val uriString: String

  override def hashCode() = 31 * uriString.## + 31 * scheme.##

  override def equals(obj: Any) = obj match {
    case that: GenericURI =>
      uriString == that.uriString && scheme == that.scheme
    case _ => false
  }

  def encode(builder: StringBuilder) =
    builder.append(uriString)

  override def toString = encode()
}

class DefaultGenericURI
(val uriString: String,
 schemeParam: Option[String] = None)
  extends GenericURI {

  private val i = uriString.indexOf(":")
  require(i != -1)
  val scheme = schemeParam.getOrElse(uriString.substring(0, i))

  val isSipURI = isInstanceOf[SipUri]


}
