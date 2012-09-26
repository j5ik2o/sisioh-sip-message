package org.sisioh.sip.util

/*
 * Copyright 2012 Sisioh Project and others. (http://www.sisioh.org/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */

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
  extends SIPDecoder[NameValuePair] with NameValuePairParser {

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

  //implicit def stringToOption(value: String): Option[String] = Some(value)

  //  def apply
  //  (name: String, value: String,
  //   separator: String = Separators.EQUALS,
  //   quotes: String = "",
  //   isQuotedString: Boolean = false): NameValuePair = apply(Some(name), Some(value), separator, quotes, isQuotedString)

  def apply
  (name: Option[String],
   value: Option[Any],
   separator: String = Separators.EQUALS,
   quotes: String = "",
   isQuotedString: Boolean = false): NameValuePair = new NameValuePair(name, value, separator, quotes, isQuotedString)

  def decode
  (source: String,
   separator: String = Separators.EQUALS,
   quotes: String = "",
   isQuotedString: Boolean = false) = new NameValuePairDecoder(separator, quotes, isQuotedString).decode(source)

  def unapply(nameValuePair: NameValuePair): Option[(Option[String], Option[Any])] =
    Some(nameValuePair.name, nameValuePair.value)

  object JsonEncoder extends Encoder[NameValuePair] {
    def encode(model: NameValuePair, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(model.value.map {
        e =>
          JField("name", JString(model.name.get)) :: JField("value", e match {
            case b: Boolean => JBool(b)
            case s: String => JString(s)
            case v: GenericObject =>
              parse(v.encodeByJson())
            case v: GenericObjectList =>
              parse(v.encodeByJson())
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


  private def formatString(source: String, stripQuotes: Boolean) =
    if (stripQuotes == false && isQuotedString) quotes + source.toString + quotes else source.toString

  def getValueAStringWithoutBoolean(stripQuotes: Boolean = true) : Option[String] = value.flatMap {
    case s: String => Some(formatString(s, stripQuotes))
    case b: Boolean => None
    case any: Any => Some(formatString(any.toString, stripQuotes))
  }

  def getValueAsString(stripQuotes: Boolean = true): Option[String] = value.flatMap {
    case s: String => Some(formatString(s, stripQuotes))
    case any: Any => Some(formatString(any.toString, stripQuotes))
  }

  private val digitRegex = """[0-9]+""".r

  def getValueAsInt = value.flatMap {
    case n: Int => Some(n)
    case digitRegex(s) => Some(s.toInt)
    case _ => None
  }

  def getValueAsBoolean = value.flatMap {
    case b: Boolean => Some(b)
    case s: String if (s.equalsIgnoreCase("true")) => Some(true)
    case s: String if (s.equalsIgnoreCase("false")) => Some(false)
    case 1 => Some(true)
    case 0 => Some(false)
    case _ => None
  }

  def getValueAsType[T](clazz: Class[T]): Option[T] = value.flatMap {
    e =>
      if (clazz.isAssignableFrom(e.getClass)) {
        Some(e.asInstanceOf[T])
      } else {
        None
      }
  }

//  def getValueAsObject(stripQuotes: Boolean): Option[String] =
//    value.map {
//      v =>
//        if (v.isInstanceOf[Boolean]) {
//          ""
//        } else if (!stripQuotes && isQuotedString) {
//          quotes + v.toString + quotes; // add the quotes for quoted string
//        } else {
//          v.toString
//        }
//    }

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

  def encodeByJson(builder: StringBuilder) = encode(builder, NameValuePair.JsonEncoder)
}
