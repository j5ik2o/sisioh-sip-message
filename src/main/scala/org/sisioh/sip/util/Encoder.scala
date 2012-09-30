package org.sisioh.sip.util

import net.liftweb.json._
import org.sisioh.sip.message.header.impl.{SIPHeader, JsonFieldNames}
import net.liftweb.json

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
 * モデルを符号化するエンコーダ。
 *
 * @tparam A モデルの型
 */
trait Encoder[A] {

  def encode(model: A, builder: StringBuilder): StringBuilder

}

trait SIPEncoder[A] extends Encoder[A]

trait JsonEncoder[A] extends Encoder[A] with JsonFieldNames {

  def encode(model: A): json.JValue

  def encode(model: A, builder: StringBuilder) =
    builder.append(compact(render(encode(model))))

  protected def getHeaderNameAsJValue[T <: SIPHeader](model: T): JField = JField(HEADER_NAME, JString(model.headerName))

}
