package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.{Host, HostPort}

/**
 * [[org.sisioh.sip.message.address.impl.SipUri]]のためのスペック。
 */
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
      val uri = """sip:KATO:password@localhost:8080"""
      "URIが取得できること" in {
        sipuri.encode() must_== uri
        sipuri.toString() must_== uri
        sipuri.uriString must_== uri
      }
      "HeaderNamesのサイズは0" in {
        sipuri.getHeaderNames.toList must have size (0)
      }
    }
    "パラメータを指定した場合" in {
      val sipuri = SipUri(authority).withParamter("PARAM1", "param1")
      val uri = """sip:KATO:password@localhost:8080;PARAM1=param1"""
      "URIが取得できること" in {
        sipuri.encode() must_== uri
        sipuri.toString() must_== uri
        sipuri.uriString must_== uri
      }
      "Parameterが取得できる" in{
        sipuri.getParameter("PARAM1") must_== Some("param1")
      }
    }
    "ヘッダーを指定した場合" in {
      val sipuri = SipUri(authority).withHeader("HEADER1", "header1")
      val uri = """sip:KATO:password@localhost:8080?HEADER1=header1"""
      "URIが取得できること" in {
        sipuri.encode() must_== uri
        sipuri.toString() must_== uri
        sipuri.uriString must_== uri
      }
      "Parameterが取得できる" in{
        sipuri.getParameter("HEADER1") must_== Some("header1")
      }
    }
  }

}
