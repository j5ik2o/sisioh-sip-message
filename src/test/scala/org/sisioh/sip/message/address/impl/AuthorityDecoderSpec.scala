package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.{Host, HostPort}

class AuthorityDecoderSpec extends Specification {

  "AuthorityDecoder" should {
    val host = "localhost"
    val port = 8080
    val userName = "KATO"
    val password = "password"
    val hostPort = HostPort(
      host = Host(host),
      port = Some(port)
    )
    val userInfo = UserInfo(
      name = userName,
      password = Some(password)
    )

    val authority = Authority(
      hostPort = Some(hostPort),
      userInfo = Some(userInfo)
    )
    val target = AuthorityDecoder()
    val encodeObject = authority.encode()
    println(encodeObject)
    val result = target.decode(encodeObject)
    "可逆的にデコードできること" in {
      result must_== authority
    }
  }

}
