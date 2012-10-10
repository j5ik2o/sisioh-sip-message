package org.sisioh.sip.message.header.impl

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

import org.sisioh.sip.core.{GenericObject, Separators}
import org.sisioh.sip.util.{JsonDecoder, Encoder}
import org.sisioh.sip.message.header._
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST.JString

object SIPHeaderJsonDecoder extends SIPHeaderJsonDecoder

class SIPHeaderJsonDecoder extends JsonDecoder[SIPHeader] {

  def decode(json: JValue) = {
    val JString(headerName) = json \ "headerName"
    headerName match {
      case CallIdHeader.NAME =>
        CallIdJsonDecoder.decode(json)
      case ContentLengthHeader.NAME =>
        ContentLengthJsonDecoder.decode(json)
      case ContentTypeHeader.NAME =>
        ContentTypeJsonDecoder.decode(json)
      case CSeqHeader.NAME =>
        CSeqJsonDecoder.decode(json)
      case ExpiresHeader.NAME =>
        ExpiresJsonDecoder.decode(json)
      case FromHeader.NAME =>
        FromJsonDecoder.decode(json)
      case MaxForwardsHeader.NAME =>
        MaxForwardsJsonDecoder.decode(json)
      case ServerHeader.NAME =>
        ServerJsonDecoder.decode(json)
      case ToHeader.NAME =>
        ToJsonDecoder.decode(json)
      case UserAgentHeader.NAME =>
        UserAgentJsonDecoder.decode(json)
      case ViaHeader.NAME =>
        ViaListJsonDecoder.decode(json)
    }
  }

}


/**
 * SIPヘッダを表すトレイト。
 */
trait SIPHeader extends GenericObject with Header {

  val headerName: String
  val name: String

  /**
   * ヘッダのボディをエンコードする。
   *
   * @return エンコードされたオブジェクト
   */
  def encodeBody(): String = encodeBody(new StringBuilder()).result()

  /**
   * ヘッダのボディをエンコードする。
   *
   * @param builder [[scala.collection.mutable.StringBuilder]]
   * @return [[scala.collection.mutable.StringBuilder]]
   */
  def encodeBody(builder: StringBuilder): StringBuilder

  /**
   * ヘッダのボディをエンコードする。
   *
   * @param encoder [[org.sisioh.sip.util.Encoder]]
   * @tparam A エンコードするモデルの型
   * @return エンコードされたオブジェクト
   */
  def encodeBody[A](encoder: Encoder[A]): String = encode(new StringBuilder, encoder).result()

  /**
   * ヘッダのボディをエンコードする。
   *
   * @param builder [[scala.collection.mutable.StringBuilder]]
   * @param encoder [[org.sisioh.sip.util.Encoder]]
   * @tparam A エンコードするモデルの型
   * @return エンコードされたオブジェクト
   */
  def encodeBody[A](builder: StringBuilder, encoder: Encoder[A]): StringBuilder = encoder.encode(this.asInstanceOf[A], builder)

  /**
   * ヘッダをエンコードする。
   *
   * ヘッダ名: ヘッダボディ[改行]の形式にエンコードする。
   *
   * @param builder [[scala.collection.mutable.StringBuilder]]
   * @return [[scala.collection.mutable.StringBuilder]]
   */
  def encode(builder: StringBuilder): StringBuilder = {
    builder.append(headerName).append(Separators.COLON).append(Separators.SP)
    encodeBody(builder)
    builder.append(Separators.NEWLINE)
    builder
  }

}
