package org.sisioh.sip.util

import collection.mutable
import org.sisioh.sip.core.Separators

object NameValuePairList {

  def apply[A](separator: String = Separators.SEMICOLON) = new NameValuePairList[A](separator)

}

class NameValuePairList[A] private
(separator: String,
 nameValuePairs: Map[String, NameValuePair[A]] = Map.empty[String, NameValuePair[A]])
  extends Iterable[NameValuePair[A]]{

  def add(nameValuePair: NameValuePair[A]): NameValuePairList[A] = {
    new NameValuePairList[A](separator, nameValuePairs + (nameValuePair.name.toLowerCase -> nameValuePair))
  }

  def add(name: String, value: A):NameValuePairList[A] = add(NameValuePair[A](name, Some(value)))

  def getNameValuePair(name: String) = nameValuePairs.get(name.toLowerCase)

  def getValue(name: String) = getNameValuePair(name).map(_.value)

  def hasNameValuePair(name: String) = getNameValuePair(name).isDefined

  def remove(name: String): NameValuePairList[A] = {
    new NameValuePairList[A](separator, nameValuePairs - name.toLowerCase)
  }

  //def isEmpty = nameValuePairs.isEmpty

  def names = nameValuePairs.keysIterator

  def values = nameValuePairs.valuesIterator

  def iterator = nameValuePairs.valuesIterator
}
