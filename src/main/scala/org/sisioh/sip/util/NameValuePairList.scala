package org.sisioh.sip.util

import org.sisioh.sip.core.{GenericObject, Separators}
import util.parsing.combinator.RegexParsers


class NameValuePairListDecoder
(separator: String = Separators.SEMICOLON,
 nameValuePairSeparator: String = Separators.EQUALS,
 quotes: String = "",
 isQuotedString: Boolean = false) extends NameValuePairListParser {

  def decode(source: String) = parseAll(nameValuePairList(separator, nameValuePairSeparator, quotes, isQuotedString), source) match {
    case Success(result, _) => result
    case Failure(msg, _) => throw new ParseException(Some(msg))
    case Error(msg, _) => throw new ParseException(Some(msg))
  }

}

trait NameValuePairListParser extends RegexParsers with NameValuePairParser {

  def nameValuePairList(separator: String, nameValuePairSeparator: String, quotes: String, isQuotedString: Boolean): Parser[NameValuePairList] =
    repsep(nameValuePair(nameValuePairSeparator, quotes, isQuotedString), separator) ^^ {
      case list =>
        val values = list.map(e => (e.name.get, e)).toMap
        new NameValuePairList(separator, values)
    }

}


object NameValuePairList {

  def apply(separator: String = Separators.SEMICOLON) = new NameValuePairList(separator)

  def decode(source: String) = NameValuePairDecoder().decode(source)

  object JsonEncoder extends Encoder[NameValuePairList] {
    def encode(model: NameValuePairList, builder: StringBuilder) = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val json = JObject(model.flatMap {
        e => e.value.map(v => JField(e.name.get, v.toString))
      }.toList)
      builder.append(compact(render(json)))

    }
  }

}

class NameValuePairList
(val separator: String = Separators.SEMICOLON,
 private val nameValuePairs: Map[String, NameValuePair] = Map.empty[String, NameValuePair])
  extends Iterable[NameValuePair] with Encodable[NameValuePairList] {

  def add(nameValuePair: NameValuePair): NameValuePairList =
    new NameValuePairList(separator, nameValuePairs + (nameValuePair.name.get.toLowerCase -> nameValuePair))

  def add(name: String, value: Any): NameValuePairList = add(NameValuePair(Some(name), Some(value)))

  def getNameValuePair(name: String) = nameValuePairs.get(name.toLowerCase)

  def getValue(name: String): Option[Any] =
    getNameValuePair(name.toLowerCase).flatMap(_.value)

  def hasNameValuePair(name: String) = getNameValuePair(name).isDefined

  def remove(name: String): NameValuePairList = {
    new NameValuePairList(separator, nameValuePairs - name.toLowerCase)
  }

  def names = nameValuePairs.keysIterator

  def values = nameValuePairs.valuesIterator

  def iterator = nameValuePairs.valuesIterator

  def getParameter(name: String): Option[String] = {
    getParameter(name, true);
  }

  def getValue(name: String, stripQuotes: Boolean): Option[String] = {
    getNameValuePair(name.toLowerCase()).flatMap(_.getValueAsObject(stripQuotes))
  }


  def getParameter(name: String, stripQuotes: Boolean): Option[String] = {
    getValue(name, stripQuotes).map {
      e =>
        if (e.isInstanceOf[GenericObject[_]]) {
          e.asInstanceOf[GenericObject[_]].encode()
        } else {
          e.toString
        }
    }
  }


  def encode(builder: StringBuilder) = {
    builder.append(this.map {
      e =>
        val encodeValue = if (e.isInstanceOf[GenericObject[_]]) {
          e.asInstanceOf[GenericObject[_]].encode()
        } else {
          e.toString()
        }
        encodeValue
    }.mkString(separator))
  }
}
