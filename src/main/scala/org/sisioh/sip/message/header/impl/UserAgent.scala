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

import org.sisioh.sip.message.header.UserAgentHeader
import org.sisioh.sip.util.{SIPDecoder, Decoder, ParserBase, Encoder}

object UserAgentDecoder extends UserAgentDecoder

class UserAgentDecoder extends SIPDecoder[UserAgent] with UserAgentParser {
  def decode(source: String) = decodeTarget(source, USER_AGENTWithCrLfOpt)
}

trait UserAgentParser extends ParserBase {
  lazy val USER_AGENTWithCrLfOpt = USER_AGENT <~ opt(CRLF)

  lazy val USER_AGENT: Parser[UserAgent] = "User-Agent" ~> HCOLON ~> rep1sep(serverVal, LWS) ^^ {
    case serverVals =>
      UserAgent(serverVals)
  }
  lazy val serverVal: Parser[ServerVal] = product | comment ^^ {
    case c => Comment(c)
  }
  lazy val product: Parser[Product] = token ~ opt(SLASH ~> productVersion) ^^ {
    case f ~ s =>
      Product(f, s)
  }
  lazy val productVersion = token
}

object UserAgent {

  def decode(source: String) = UserAgentDecoder.decode(source)

  object JsonEncoder extends Encoder[UserAgent] {
    def encode(model: UserAgent, builder: StringBuilder) = {
      import net.liftweb.json._
      val list = model.serverVals.map {
        e => JString(e.toString)
      }.toList
      val json = JObject(
        JField("headerName", JString(model.headerName)) ::
          JField("serverVals", JArray(list)) :: Nil
      )
      builder.append(compact(render(json)))
    }
  }

}

/**
 * [[org.sisioh.sip.message.header.impl.Product]]もしくは
 * [[org.sisioh.sip.message.header.impl.Comment]]を表すトレイト。。
 */
trait ServerVal

/**
 * コメントを表す値オブジェクト。
 *
 * @param text コメント
 */
case class Comment(text: String) extends ServerVal

/**
 * [[org.sisioh.sip.message.header.impl.Product]]のコンパニオンオブジェクト。
 */
object Product {

  /**
   * PRODUCT/1.0形式の文字列をパースしインスタンスを生成する。
   *
   * @param fullName PRODUCT/1.0形式の文字列
   * @return [[org.sisioh.sip.message.header.impl.Product]]
   */
  def from(fullName: String) = {
    val sp = fullName.split("/")
    if (sp.size == 2) {
      Product(sp(0), Some(sp(1)))
    } else if (sp.size == 1) {
      Product(sp(0))
    } else {
      throw new IllegalArgumentException
    }
  }

}

/**
 * プロダクトを表す値オブジェクト。
 *
 * @param name プロダクト名
 * @param version プロダクトバージョン
 */
case class Product(name: String, version: Option[String] = None) extends ServerVal {
  override def toString = List(Some(name), version).flatten.mkString("/")
}

/**
 * ユーザエージェントを表す値オブジェクト。
 *
 * @param serverVals [[org.sisioh.sip.message.header.impl.ServerVal]]のリスト
 */
case class UserAgent(serverVals: List[ServerVal] = List.empty)
  extends SIPHeader with UserAgentHeader {

  val headerName = UserAgentHeader.NAME

  def addProduct(product: Product): UserAgent = {
    UserAgent(
      product :: serverVals
    )
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, UserAgent.JsonEncoder)

  private def encodeProducts(builder: StringBuilder): StringBuilder = {
    builder.append(serverVals.mkString(" "))
  }

  def encodeBody(builder: StringBuilder) = encodeProducts(builder)

  override def toString = encode()
}
