package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification

class UserInfoDecoderSpec extends Specification {

  "UserInfoDecoder" should {
    val userInfo = UserInfo("kato", Some("pass"))
    val encode = userInfo.encode()
    val ui = UserInfoDecoder().decode(encode + "@")
    "可逆的にデコードできること" in {
      userInfo must_== ui
    }
  }

}
