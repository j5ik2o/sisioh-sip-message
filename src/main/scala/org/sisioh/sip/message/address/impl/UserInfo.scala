package org.sisioh.sip.message.address.impl

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
import org.sisioh.sip.util._
import util.parsing.combinator.RegexParsers
import scala.Some

object UserInfoDecoder extends UserInfoDecoder

class UserInfoDecoder extends SIPDecoder[UserInfo] with UserInfoParser {
  def decode(source: String) = decodeTarget(source, userInfo)
}

trait UserInfoParser extends RegexParsers with ParserBase {

  lazy val userInfo: Parser[UserInfo] = user ~ opt(elem(':') ~> password) ^^ {
    case user ~ passwordOp =>
      UserInfo(user, passwordOp)
  }

  lazy val userInfoWithAt: Parser[UserInfo] = userInfo <~ '@'

  lazy val password = rep(unreserved | escaped | '&' | '=' | '+' | '$' | ',') ^^ {
    _.mkString
  }

  lazy val userUnreserved: Parser[Char] = elem('&') | '=' | '+' | '$' | ',' | ';' | '?' | '/'

  lazy val user: Parser[String] = rep1(unreserved | escaped | userUnreserved) ^^ {
    _.mkString
  }

}


object UserType extends Enumeration {
  val TELHPHONE_SUBSCRIBER, USER = Value
}

/**
 * [[org.sisioh.sip.message.address.impl.UserInfo]]のためのコンパニオンオブジェクト。
 */
object UserInfo {

  /**
   * ファクトリメソッド。
   *
   * @param name ユーザ名
   * @param password パスワードのオプション
   * @param userTypeParam [[org.sisioh.sip.message.address.impl.UserType.Value]]のオブション
   * @return [[org.sisioh.sip.message.address.impl.UserInfo]]
   */
  def apply(name: String, password: Option[String], userTypeParam: Option[UserType.Value] = None) =
    new UserInfo(name, password, userTypeParam)

  /**
   * 抽出子メソッド。
   *
   * @param userInfo [[org.sisioh.sip.message.address.impl.UserInfo]]
   * @return Option[(ユーザ名, パスワードのオプション, [[org.sisioh.sip.message.address.impl.UserType.Value]]のオプション)]
   */
  def unapply(userInfo: UserInfo): Option[(String, Option[String], Option[UserType.Value])] =
    Some(userInfo.name, userInfo.password, Some(userInfo.userType))

  object JsonEncoder extends Encoder[UserInfo] {

    def encode(model: UserInfo, builder: StringBuilder): StringBuilder = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val json = JObject(model.password.map {
        e =>
          JField("name", model.name) :: JField("password", JString(e)) :: Nil
      }.getOrElse {
        JField("name", model.name) :: Nil
      })
      builder.append(compact(render(json)))
    }

  }

}

/**
 * ユーザ情報を表す値オブジェクト。
 *
 * @param name ユーザ名
 * @param password パスワードのオプション
 * @param userTypeParam [[org.sisioh.sip.message.address.impl.UserType.Value]]のオブション
 */
class UserInfo
(val name: String,
 val password: Option[String],
 userTypeParam: Option[UserType.Value] = None)
  extends GenericObject {

  require(name.size > 0)
  require {
    password.map {
      _.size > 0
    }.getOrElse(true)
  }

  val userType = userTypeParam.getOrElse {
    if (name.indexOf(Separators.POUND) >= 0 || name.indexOf(Separators.SEMICOLON) >= 0) {
      UserType.TELHPHONE_SUBSCRIBER
    } else {
      UserType.USER
    }
  }

  def encode(builder: StringBuilder) = {
    builder.append(password.map {
      e =>
        "%s%s%s".format(name, Separators.COLON, e)
    }.getOrElse {
      name
    })
  }

  override def toString = encode()

  override def hashCode() =
    31 * name.## + 31 * password.## + 31 * userType.##

  override def equals(obj: Any) = obj match {
    case that: UserInfo =>
      name == that.name && password == that.password && userType == that.userType
    case _ => false
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, UserInfo.JsonEncoder)
}
