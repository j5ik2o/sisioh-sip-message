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


import org.sisioh.sip.util._
import org.sisioh.sip.core.{GenericObject, Separators}
import net.liftweb.json._
import scala.Some

object AuthorityDecoder {
  def apply() = new AuthorityDecoder
}

class AuthorityDecoder extends SIPDecoder[Authority] with AuthorityParser {
  def decode(source: String) = decodeTarget(source, authority)
}


trait AuthorityParser extends ParserBase with UserInfoParser with HostPortParser {

  lazy val regName: Parser[String] = rep1(unreserved | escaped | '$' | ',' | ';' | ':' | '@' | '&' | '=' | '+') ^^ {
    _.mkString
  }

  lazy val authority: Parser[Authority] = srvr ^^ {
    case s =>
      Authority(Some(s._2), s._1)
  }

  lazy val srvr: Parser[(Option[UserInfo], HostPort)] = opt(userInfoWithAt) ~ hostPort ^^ {
    case userInfoOpt ~ hostPort =>
      (userInfoOpt, hostPort)
  }
}

object AuthorityEncoder extends SIPEncoder[Authority] {

  def encode(model: Authority, builder: StringBuilder) = {
    (model.hostPort, model.userInfo) match {
      case (Some(hp), Some(ui)) =>
        ui.encode(builder).append(Separators.AT)
        hp.encode(builder)
        builder
      case (Some(hp), None) =>
        hp.encode(builder)
        builder
      case _ =>
        builder
    }
  }

}

object AuthorityJsonDecoder extends JsonDecoder[Authority] {

  def decode(json: JsonAST.JValue): Authority = {
    val hostPortOpt = (json \ "hostPort").toOpt.map {
      HostPortJsonDecoder.decode(_)
    }
    val userInfoOpt = (json \ "userInfo").toOpt.map {
      UserInfoJsonDecoder.decode(_)
    }
    Authority(hostPortOpt, userInfoOpt)
  }

}

object AuthorityJsonEncoder extends JsonEncoder[Authority] {

  def encode(model: Authority) = {
    (model.hostPort, model.userInfo) match {
      case (Some(hp), Some(ui)) =>
        JObject(JField("hostPort", hp.encodeAsJValue()) :: JField("userInfo", ui.encodeAsJValue()) :: Nil)
      case (Some(hp), None) =>
        JObject(JField("hostPort", hp.encodeAsJValue()) :: Nil)
      case (None, Some(ui)) =>
        JObject(JField("userInfo", ui.encodeAsJValue()) :: Nil)
      case _ =>
        JNull
    }
  }

}


/**
 * [[org.sisioh.sip.message.address.impl.Authority]]のためのコンパニオンオブジェクト。
 */
object Authority {

  def apply(hostPort: Option[HostPort], userInfo: Option[UserInfo]): Authority = new Authority(hostPort, userInfo)

  def decode(source: String) = AuthorityDecoder().decode(source)

}

/**
 * ユーザ情報とホスト情報を併せ持つ
 *
 * @param hostPort [[org.sisioh.sip.util.HostPort]]のオプション
 * @param userInfo [[org.sisioh.sip.message.address.impl.UserInfo]]のオプション
 */
class Authority(val hostPort: Option[HostPort], val userInfo: Option[UserInfo]) extends GenericObject {
  val userName = userInfo.map(_.name)
  val host = hostPort.map(_.host)
  val port: Option[Int] = hostPort.flatMap(_.port)

  def removePort: Authority = Authority(hostPort.map(_.removePort), userInfo)

  def removeUserInfo: Authority = Authority(hostPort, None)

  def encode(builder: StringBuilder): StringBuilder =
    AuthorityEncoder.encode(this, builder)

  def encodeAsJValue() = AuthorityJsonEncoder.encode(this)

  override def hashCode() = 31 * hostPort.## + 31 * userInfo.##

  override def equals(obj: Any) = obj match {
    case that: Authority =>
      hostPort == that.hostPort && userInfo == that.userInfo
    case _ => false
  }

}
