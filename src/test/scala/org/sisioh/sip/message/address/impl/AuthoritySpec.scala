package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.{Host, HostPort}

class AuthoritySpec extends Specification {

  "Authority" should {
    val host = "localhost"
    val port = 8080
    val userName = "KATO"
    val password = "password"
    val authority = Authority(
      hostPort = Some(
        HostPort(
          host = Host(host),
          port = Some(port)
        )
      ),
      userInfo = Some(
        UserInfo(
          name = userName,
          password = Some(password)
        )
      )
    )
    "ホスト名を取得できる" in {
      authority.host must_== Some(Host(host))
    }
    "ポート番号を取得できる" in {
      authority.port must_== Some(port)
    }
    "エンコード文字列を取得できる" in {
      authority.encode() must_== """KATO:password@localhost:8080"""
    }
    "HostPortが取得できる" in {
      authority.hostPort must_== Some(HostPort(Host(host), Some(port)))
    }
  }


}
