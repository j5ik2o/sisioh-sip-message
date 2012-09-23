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
    "エンコード文字列を取得できる" in {
      val org = address.encode().getBytes.length
      val json = address.encodeByJson().getBytes.length
      val r = BigDecimal(json) / BigDecimal(org)
      println("raito = %.2f".format(r))
      address.encode() must_== """"かとう" <sip:KATO:password@localhost:8080>"""
      address.encodeByJson() must_== """{"uri":{"scheme":"sip","authority":{"hostPort":{"host":"localhost","port":8080},"userInfo":{"name":"KATO","password":"password"}},"uriParams":{},"qheaders":{}},"displayName":"かとう","addressType":0}"""
    }
  }

}
