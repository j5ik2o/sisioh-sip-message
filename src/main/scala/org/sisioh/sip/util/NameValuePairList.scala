package org.sisioh.sip.util

import org.sisioh.sip.core.{GenericObjectList, GenericObject, Separators}
import util.parsing.combinator.RegexParsers

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
 isQuotedString: Boolean = false) extends Decoder with NameValuePairListParser {

  def decode(source: String): NameValuePairList =
    decodeTarget(source, nameValuePairList(separator, nameValuePairSeparator, quotes, isQuotedString))

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

  def apply(separator: String = Separators.SEMICOLON): NameValuePairList = new NameValuePairList(separator)

  def decode(source: String) = NameValuePairDecoder().decode(source)

  def fromValues(nameValuePairs: List[NameValuePair], separator: String = Separators.SEMICOLON): NameValuePairList = {
    new NameValuePairList(separator, nameValuePairs.map(e => (e.name.get, e)).toMap)
  }

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
  extends Iterable[NameValuePair] with GenericObjectList {

  def add(nameValuePairList: NameValuePairList) =
    new NameValuePairList(separator, nameValuePairs ++ nameValuePairList.nameValuePairs)

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

  def getParameter(name: String): Option[String] =
    getParameter(name, true);

  def getValue(name: String, stripQuotes: Boolean): Option[String] =
    getNameValuePair(name.toLowerCase()).flatMap(_.getValueAsObject(stripQuotes))


  def getParameter(name: String, stripQuotes: Boolean): Option[String] = {
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

  override def hashCode() = 31 * separator.## + 31 * nameValuePairs.##

  override def equals(obj: Any) = obj match {
    case that: NameValuePairList =>
      separator == that.separator && nameValuePairs == that.nameValuePairs
    case _ => false
  }

  override def toString = encode()

  def encodeByJson(builder: StringBuilder) = encode(builder, NameValuePairList.JsonEncoder)
}
