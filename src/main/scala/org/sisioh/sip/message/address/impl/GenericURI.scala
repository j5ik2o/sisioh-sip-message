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

  lazy val genericURI: Parser[GenericURI] = absoluteURI

  lazy val absoluteURI: Parser[GenericURI] = scheme ~ ":" ~ (hierPart | opaquePart) ^^ {
    case scheme ~ colon ~ part =>
      DefaultGenericURI(scheme + colon + part)
  }

  lazy val scheme: Parser[String] = ALPHA ~ rep(ALPHA | DIGIT | '+' | '-' | '.') ^^ {
    case f ~ s =>
      new StringBuilder().append(f).append(s.mkString).result()
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
      new StringBuilder().append(uns).append(urics.mkString).result()
  }

  lazy val uricNoSlash: Parser[Char] = unreserved | escaped | elem(';') | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ','

  lazy val uric: Parser[Char] = reserved | unreserved | escaped

  lazy val netPath: Parser[String] = "//" ~ authority ~ "@" ~ opt(absPath) ^^ {
    case s ~ a ~ at ~ opt =>
      s + a.toString + at + opt.getOrElse("")
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

  lazy val pchar: Parser[Char] = unreserved | escaped | elem(':') | '@' | '&' | '=' | '+' | '$' | ','
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

  object JsonEncoder extends Encoder[GenericURI] {
    def encode(model: GenericURI, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("uriString", JString(model.uriString)) :: Nil)
      builder.append(compact(render(json)))
    }
  }
}



object WildCardURI extends WildCardURI {

}

class WildCardURI extends GenericURI {
  val uriString = "*"
  val scheme = ""
  val isSipURI = false

  def encodeByJson(builder: StringBuilder) = encode(builder, JsonEncoder)
}

class DefaultGenericURI
(val uriString: String,
 schemeParam: Option[String] = None)
  extends GenericURI {

  private val i = uriString.indexOf(":")
  require(i != -1)
  val scheme = schemeParam.getOrElse(uriString.substring(0, i))

  val isSipURI = isInstanceOf[SipUri]

  def encodeByJson(builder: StringBuilder) = encode(builder, JsonEncoder)
}
