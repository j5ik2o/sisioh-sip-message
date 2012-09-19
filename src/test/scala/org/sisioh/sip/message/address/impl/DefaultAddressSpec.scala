package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.{Host, HostPort}

class DefaultAddressSpec extends Specification {

  "DefaultAddress" should {
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
    val sipuri = SipUri(authority)
    val address = DefaultAddress.fromURI(sipuri, Some("かとう"))
    println(address.encode())
    true must_== true
  }

}
