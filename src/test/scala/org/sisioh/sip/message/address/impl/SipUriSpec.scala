package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.{Host, HostPort}

class SipUriSpec extends Specification {

  "SipUri" should {
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
    "Authorityのみの場合" in {
      val sipuri = SipUri(authority)
      sipuri.encode() must_== """sip:KATO:password@localhost:8080"""
      "HeaderNamesのサイズは0" in {
        sipuri.getHeaderNames.toList must have size (0)
      }
    }
    "パラメータを指定した場合" in {
      val sipuri = SipUri(authority).withParamter("PARAM1", "param1")
      sipuri.encode() must_== """sip:KATO:password@localhost:8080;PARAM1=param1"""
      sipuri.getParameter("PARAM1") must beSome
    }
    "ヘッダーを指定した場合" in {
      val sipuri = SipUri(authority).withHeader("HEADER1", "header1")
      sipuri.encode() must_== """sip:KATO:password@localhost:8080?HEADER1=header1"""
      sipuri.getHeader("HEADER1") must beSome
    }
  }

}
