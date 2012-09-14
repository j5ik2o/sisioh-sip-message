package org.sisioh.sip.message.address.impl

import org.sisioh.sip.core.Separators
import org.sisioh.sip.util.{Encoder, Encodable}

object UserType extends Enumeration {
  val TELHPHONE_SUBSCRIBER, USER = Value
}

object UserInfo {

  def apply(name: String, password: Option[String], userTypeParam: Option[UserType.Value] = None) =
    new UserInfo(name, password, userTypeParam)

  def unapply(userInfo: UserInfo): Option[(String, Option[String], Option[UserType.Value])] =
    Some(userInfo.name, userInfo.password, Some(userInfo.userType))

  implicit object DefaultUserInfoEncoder extends Encoder[UserInfo] {

    def encode(model: UserInfo, builder: StringBuilder): StringBuilder = {
      model.password.map {
        builder.append(model.name).append(Separators.COLON).append(_)
      }.getOrElse {
        builder.append(model.name)
      }
    }

  }

}

class UserInfo
(val name: String,
 val password: Option[String],
 userTypeParam: Option[UserType.Value] = None)
  extends Encodable[UserInfo] {

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

  override def hashCode() = {
    31 * name.## + 31 * password.## + 31 * userType.##
  }

  override def equals(obj: Any) = obj match {
    case that: UserInfo =>
      if (userType != that.userType) {
        false
      } else if (name.equalsIgnoreCase(that.name) == false) {
        false
      } else {
        (password, that.password) match {
          case (Some(_), None) => false
          case (None, Some(_)) => false
          case (l, r) if (l.eq(r)) => true
          case (l, r) if (l == r) => true
          case _ => false
        }
      }
    case _ => false
  }
}
