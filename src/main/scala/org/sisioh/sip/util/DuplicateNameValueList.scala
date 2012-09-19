package org.sisioh.sip.util

import scala.collection.mutable._

object DuplicateNameValueList {

  def apply() = new DuplicateNameValueList(new HashMap[String, Set[NameValuePair]] with MultiMap[String, NameValuePair])

}

class DuplicateNameValueList
(val nameValuePairs: HashMap[String, Set[NameValuePair]] with MultiMap[String, NameValuePair])
  extends Iterable[Set[NameValuePair]] {

  def add(nameValuePair: NameValuePair): DuplicateNameValueList =
    new DuplicateNameValueList(nameValuePairs.addBinding(nameValuePair.name.get, nameValuePair))

  def add(name: String, value: Any): DuplicateNameValueList = add(NameValuePair(Some(name), Some(value)))

  def getNameValuePairs(name: String): Set[NameValuePair] = nameValuePairs(name.toLowerCase)

  def getValue(name: String): Set[NameValuePair] = getNameValuePairs(name.toLowerCase)

  def hasNameValuePair(name: String) = nameValuePairs.contains(name)

  def remove(name: String): DuplicateNameValueList = {
    nameValuePairs.remove(name).map(e => new DuplicateNameValueList(nameValuePairs)).getOrElse(this)
  }

  def names = nameValuePairs.keysIterator

  def values = nameValuePairs.valuesIterator

  def iterator = nameValuePairs.valuesIterator
}
