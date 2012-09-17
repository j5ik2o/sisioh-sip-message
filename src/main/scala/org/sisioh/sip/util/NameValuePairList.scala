package org.sisioh.sip.util

import collection.mutable
import org.sisioh.sip.core.{GenericObject, Separators}

object NameValuePairList {

  def apply(separator: String = Separators.SEMICOLON) = new NameValuePairList(separator)

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

class NameValuePairList private
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

  def encode(builder: StringBuilder) = {
    builder.append(this.map{
      e =>
        val encodeValue = if (e.isInstanceOf[GenericObject[_]]){
          e.asInstanceOf[GenericObject[_]].encode()
        } else {
          e.toString()
        }
        encodeValue
    }.mkString(separator))
  }
}
