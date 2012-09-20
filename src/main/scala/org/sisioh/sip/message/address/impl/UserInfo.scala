package org.sisioh.sip.message.address.impl

import org.sisioh.sip.core.{GenericObject, Separators}
import org.sisioh.sip.util.{Decoder, ParserBase, Encoder, Encodable}
import util.parsing.combinator.RegexParsers

object UserInfoDecoder {
  def apply() = new UserInfoDecoder
}

class UserInfoDecoder extends Decoder[UserInfo] with UserInfoParser {
  def decode(source: String) = decodeTarget(source, userInfo)
}

trait UserInfoParser extends RegexParsers with ParserBase {
  def userUnreserved: Parser[String] = "&" | "=" | "+" | "$" | "," | ";" | "?" | "/"

  def userInfo: Parser[UserInfo] = user ~ opt(":" ~> password) <~ "@" ^^ {
    case user ~ passwordOp =>
      UserInfo(user, passwordOp)
  }

  def password = rep(unreserved | escaped | "&" | "=" | "+" | "$" | ",") ^^ {
    v => v.mkString
  }

  def user: Parser[String] = rep1(unreserved | escaped | userUnreserved) ^^ {
    v => v.mkString
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

  class JsonEncoder extends Encoder[UserInfo] {

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

}
