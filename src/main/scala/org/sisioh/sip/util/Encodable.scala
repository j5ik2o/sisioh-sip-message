package org.sisioh.sip.util

import net.liftweb.json
import net.liftweb.json._

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

/**
 * エンコードできる能力を表すトレイト。
 */
trait Encodable {

  def encode(): String = encode(new StringBuilder).result()
  def encode(builder: StringBuilder): StringBuilder
  def encode[A](encoder: Encoder[A]): String = encode(new StringBuilder, encoder).result()

  def encodeByJson(): String = encodeByJson(new StringBuilder).result()
  def encodeByJson(builder: StringBuilder): StringBuilder = builder.append(compact(render(encodeAsJValue)))
  def encodeAsJValue(): json.JValue

  def encode[A](builder: StringBuilder, encoder: Encoder[A]):StringBuilder = encoder.encode(this.asInstanceOf[A], builder)

}
