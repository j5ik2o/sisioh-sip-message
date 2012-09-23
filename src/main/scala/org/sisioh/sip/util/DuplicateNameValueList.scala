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

import scala.collection.mutable._

object DuplicateNameValueList {

  def apply() = new DuplicateNameValueList(new HashMap[String, Set[NameValuePair]] with MultiMap[String, NameValuePair])

}

class DuplicateNameValueList
(val nameValuePairs: HashMap[String, Set[NameValuePair]] with MultiMap[String, NameValuePair])
  extends Iterable[Set[NameValuePair]] {

  override def hashCode() = 31 * nameValuePairs.##

  override def equals(obj: Any) = obj match {
    case that: DuplicateNameValueList =>
      nameValuePairs == that.nameValuePairs
    case _ =>
      false
  }

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
