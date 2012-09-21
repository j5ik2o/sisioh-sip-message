package org.sisioh.sip.util

import scala.Tuple2
import org.sisioh.sip.core.{GenericObjectList, GenericObject, Separators}
import util.parsing.combinator.RegexParsers
import javax.swing.plaf.OptionPaneUI

object NameValuePairDecoder {

  def apply(separator: String = Separators.EQUALS,
            quotes: String = "",
            isQuotedString: Boolean = false) = new NameValuePairDecoder(separator, quotes, isQuotedString)

}


class NameValuePairDecoder
(separator: String,
 quotes: String,
 isQuotedString: Boolean)
  extends Decoder with NameValuePairParser {

  def decode(source: String): NameValuePair = decodeTarget(source, nameValuePair(separator, quotes, isQuotedString))
}

trait NameValuePairParser extends RegexParsers {

  def nameValuePair(separator: String, quotes: String, isQuotedString: Boolean): Parser[NameValuePair] =
    NAME ~ separator ~ quotes ~ VALUE ~ quotes ^^ {
      case name ~ sp ~ lq ~ value ~ rq =>
        NameValuePair(Some(name), Some(value), sp, lq)
    } ||| NAME ~ separator ~ quotes ~ quotes ^^ {
      case name ~ sp ~ lq ~ rq =>
        NameValuePair(Some(name), Some(""), sp, lq, true)
    } ||| NAME ~ separator ^^ {
      case name ~ sp =>
        NameValuePair(Some(name), Some(""), sp, "", false)
    }

  lazy val NAME = """[a-zA-Z.]+""".r
  lazy val VALUE = """[a-zA-Z.]+""".r

}

object NameValuePair {

  def apply
  (name: Option[String],
   value: Option[Any],
   separator: String = Separators.EQUALS,
   quotes: String = "",
   isQuotedString: Boolean = false) = new NameValuePair(name, value, separator, quotes, isQuotedString)

  def decode
  (source: String,
   separator: String = Separators.EQUALS,
   quotes: String = "",
   isQuotedString: Boolean = false) = new NameValuePairDecoder(separator, quotes, isQuotedString).decode(source)

  def unapply(nameValuePair: NameValuePair): Option[(Option[String], Option[Any])] =
    Some(nameValuePair.name, nameValuePair.value)

  class JsonEncoder[A] extends Encoder[NameValuePair] {
    def encode(model: NameValuePair, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(model.value.map {
        e =>
          JField("name", JString(model.name.get)) :: JField("value", e match {
            case s: String => JString(s)
            case v: GenericObject =>
              parse(v.encode())
            case v: GenericObjectList =>
              parse(v.encode())
          }) :: Nil
      }.getOrElse {
        JField("name", JString(model.name.get)) :: Nil
      })
      builder.append(compact(render(json)))
    }
  }

}


class NameValuePair
(val name: Option[String],
 val value: Option[Any],
 val separator: String = Separators.EQUALS,
 val quotes: String = "",
 val isQuotedString: Boolean = false)
  extends Tuple2[Option[String], Option[Any]](name, value) with GenericObject {

  def getValueAsObject(stripQuotes: Boolean): Option[String] =
    value.map {
      v =>
        if (v.isInstanceOf[Boolean]) {
          ""
        } else if (!stripQuotes && isQuotedString) {
          quotes + v.toString() + quotes; // add the quotes for quoted string
        } else {
          v.toString
        }
    }

  override def hashCode() =
    31 * name.## + 31 * value.##

  override def equals(obj: Any) = obj match {
    case that: NameValuePair =>
      name == that.name && value == that.value
    case _ => false
  }

  override def toString() = encode()

  def encode(builder: StringBuilder) = {
    (name, value) match {
      case (Some(n), Some(v)) if (v.isInstanceOf[Boolean] == false) =>
        if (v.isInstanceOf[GenericObject]) {
          builder.append(n).append(separator).append(quotes)
          v.asInstanceOf[GenericObject].encode(builder)
          builder.append(quotes)
        } else if (v.isInstanceOf[GenericObjectList]) {
          builder.append(n).append(separator).append(v.asInstanceOf[GenericObjectList].encode())
        } else if (v.toString.size == 0) {
          if (isQuotedString) {
            builder.append(n).append(separator).append(quotes).append(quotes)
          } else {
            builder.append(n).append(separator)
          }
        } else {
          builder.append(n).append(separator).append(quotes).append(v.toString).append(quotes)
        }
      case (None, Some(v)) =>
        if (v.isInstanceOf[GenericObject]) {
          builder.append(v.asInstanceOf[GenericObject].encode())
        } else if (v.isInstanceOf[GenericObjectList]) {
          builder.append(v.asInstanceOf[GenericObjectList].encode())
        } else {
          builder.append(quotes).append(v.toString).append(quotes)
        }
      case (Some(n), Some(v)) if (v.isInstanceOf[Boolean] && v.asInstanceOf[Boolean]) =>
        builder.append(n)
      case _ =>
        builder
    }

  }
}
