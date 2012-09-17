package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification

class UserInfoSpec extends Specification {

  "userInfo" should {
    "ユーザ名を指定した場合" in {
      val bob = UserInfo("bob", Some("abc"))
      "ユーザタイプはユーザである" in {
        bob.name must_== "bob"
        bob.password must_== Some("abc")
        bob.userType must_== UserType.USER
        bob.encode() must_== """bob:abc"""
      }
    }
    "電話番号購読者" in {
      val phoneNumber = UserInfo("0#1;", None)
      "ユーザタイプは電話番号購読者である" in {
        phoneNumber.name must_== "0#1;"
        phoneNumber.password must_== None
        phoneNumber.userType must_== UserType.TELHPHONE_SUBSCRIBER
        phoneNumber.encode() must_== """0#1;"""
      }

    }

  }


}
