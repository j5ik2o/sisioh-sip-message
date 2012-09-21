package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.URI
import org.sisioh.sip.util._
import org.sisioh.sip.core.GenericObject
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
class DefaultGenericURIDecoder extends Decoder with DefaultGenericURIParser {

  def decode(source: String): GenericURI = decodeTarget(source, genericURI)

}

trait DefaultGenericURIParser extends ParserBase with AuthorityParser {

  lazy val genericURI: Parser[GenericURI] = absoluteURI ^^ {
    uri => DefaultGenericURI(uri)
  }

  lazy val absoluteURI: Parser[String] = scheme ~ ":" ~ (hierPart | opaquePart) ^^ {
    case scheme ~ colon ~ part =>
      scheme + colon + part
  }

  lazy val scheme: Parser[String] = ALPHA ~ rep(ALPHA | DIGIT | "+" | "-" | ".") ^^ {
    case f ~ s =>
      f + s.mkString
  }

  lazy val hierPart: Parser[String] = (netPath | absPath) ~ opt("?" ~ query) ^^ {
    case path ~ queryOpt =>
      path + queryOpt.map {
        case f ~ s => f + s
      }.getOrElse("")
  }

  lazy val query: Parser[String] = rep(uric) ^^ {
    _.mkString
  }

  lazy val opaquePart: Parser[String] = uricNoSlash ~ rep(uric) ^^ {
    case uns ~ urics =>
      uns + urics.mkString
  }

  lazy val uricNoSlash: Parser[String] = unreserved | escaped | ";" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ","

  lazy val uric: Parser[String] = reserved | unreserved | escaped

  lazy val netPath: Parser[String] = "//" ~ authority ~ "@" ~ opt(absPath) ^^ {
    case slash ~ authority ~ at ~ absPathOpt =>
      slash + authority.toString + at + absPathOpt.getOrElse("")
  }

  lazy val absPath: Parser[String] = "/" ~ pathSegments ^^ {
    case f ~ s =>
      f + s
  }

  lazy val pathSegments: Parser[String] = rep1sep(segment, "/") ^^ {
    _.mkString("/")
  }

  lazy val segment: Parser[String] = rep1sep(param, ";") ^^ {
    _.mkString(";")
  }

  lazy val param: Parser[String] = rep(pchar) ^^ {
    _.mkString
  }

  lazy val pchar = unreserved | escaped | ":" | "@" | "&" | "=" | "+" | "$" | ","
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

object WildCardURI extends WildCardURI

class WildCardURI extends GenericURI {
  val uriString = "*"
  val scheme = ""
  val isSipURI = false
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
