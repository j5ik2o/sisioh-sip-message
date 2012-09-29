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

import org.sisioh.sip.core.{GenericObjectList, GenericObject, Separators}
import util.parsing.combinator.RegexParsers
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

object NameValuePairListDecoder {
  def apply
  (separator: String = Separators.SEMICOLON,
   nameValuePairSeparator: String = Separators.EQUALS,
   quotes: String = "",
   isQuotedString: Boolean = false) = new NameValuePairListDecoder
}

class NameValuePairListDecoder
(separator: String = Separators.SEMICOLON,
 nameValuePairSeparator: String = Separators.EQUALS,
 quotes: String = "",
 isQuotedString: Boolean = false)
  extends SIPDecoder[NameValuePairList] with NameValuePairListParser {

  def decode(source: String): NameValuePairList =
    decodeTarget(source, nameValuePairList(separator, nameValuePairSeparator, quotes, isQuotedString))

}

trait NameValuePairListParser extends RegexParsers with NameValuePairParser {

  def nameValuePairList
  (separator: String,
   nameValuePairSeparator: String,
   quotes: String,
   isQuotedString: Boolean): Parser[NameValuePairList] =
    repsep(nameValuePair(nameValuePairSeparator, quotes, isQuotedString), separator) ^^ {
      case list =>
        val values = list.map(e => (e.name.get, e)).toMap
        new NameValuePairList(separator, values)
    }

}


object NameValuePairListJsonDecoder extends JsonDecoder[NameValuePairList] {
  def decode(json: JValue) = {
    val JString(separator) = json \ "separator"
    val l = (json \ "values").children.map {
      v =>
        NameValuePair(Some(v.asInstanceOf[JField].name), Some(v.asInstanceOf[JField].value))
    }
    NameValuePairList.fromValues(l, separator)
  }
}

object NameValuePairListJsonEncoder extends JsonEncoder[NameValuePairList] {

  def encode(model: NameValuePairList) = {
    JObject(
      JField("separator", JString(model.separator)) ::
        JField("values",
          model.flatMap {
            e => e.value.map(v => JField(e.name.get, v.toString))
          }.toList) :: Nil
    )
  }

}

object NameValuePairList {

  def apply(separator: String = Separators.SEMICOLON): NameValuePairList = new NameValuePairList(separator)

  def decode(source: String) = NameValuePairDecoder().decode(source)

  def fromValues(nameValuePairs: List[NameValuePair], separator: String = Separators.SEMICOLON): NameValuePairList = {
    new NameValuePairList(separator, nameValuePairs.map(e => (e.name.get, e)).toMap)
  }

}

class NameValuePairList
(val separator: String = Separators.SEMICOLON,
 private val nameValuePairs: Map[String, NameValuePair] = Map.empty[String, NameValuePair])
  extends Iterable[NameValuePair] with GenericObjectList {

  def add(nameValuePairList: NameValuePairList) =
    new NameValuePairList(separator, nameValuePairs ++ nameValuePairList.nameValuePairs)

  def add(nameValuePair: NameValuePair): NameValuePairList =
    new NameValuePairList(separator, nameValuePairs + (nameValuePair.name.get.toLowerCase -> nameValuePair))

  def add(name: String, value: Any): NameValuePairList = add(NameValuePair(Some(name), Some(value)))

  def getNameValuePair(name: String): Option[NameValuePair] = nameValuePairs.get(name.toLowerCase)

  def getValue(name: String): Option[Any] =
    getNameValuePair(name.toLowerCase).flatMap(_.value)

  def getValueAsType[T](clazz: Class[T], name: String): Option[T] = getNameValuePair(name).flatMap(_.getValueAsType(clazz))

  def getValueAsString(name: String, stripQuotes: Boolean = true): Option[String] = getNameValuePair(name).flatMap(_.getValueAsString(stripQuotes))

  def getValueAsInt(name: String): Option[Int] = getNameValuePair(name).flatMap(_.getValueAsInt)

  def hasNameValuePair(name: String) = getNameValuePair(name).isDefined

  def remove(name: String): NameValuePairList = {
    new NameValuePairList(separator, nameValuePairs - name.toLowerCase)
  }

  def names = nameValuePairs.keysIterator

  def values = nameValuePairs.valuesIterator

  def iterator = nameValuePairs.valuesIterator

  def getValue(name: String, stripQuotes: Boolean = true): Option[String] =
    getNameValuePair(name.toLowerCase).flatMap(_.getValueAStringWithoutBoolean(stripQuotes))


  def getParameter(name: String, stripQuotes: Boolean = true): Option[String] = {
    getValue(name, stripQuotes).map {
      e =>
        if (e.isInstanceOf[GenericObject]) {
          e.asInstanceOf[GenericObject].encode()
        } else {
          e.toString
        }
    }
  }

  def encode(builder: StringBuilder) = {
    builder.append(this.map {
      e =>
        val encodeValue = if (e.isInstanceOf[GenericObject]) {
          e.asInstanceOf[GenericObject].encode()
        } else {
          e.toString()
        }
        encodeValue
    }.mkString(separator))
  }

  def encodeAsJValue() = NameValuePairListJsonEncoder.encode(this)

  override def hashCode() = 31 * separator.## + 31 * nameValuePairs.##

  override def equals(obj: Any) = obj match {
    case that: NameValuePairList =>
      separator == that.separator && nameValuePairs == that.nameValuePairs
    case _ => false
  }

  override def toString() = encode()

}
