package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.SIPConstants
import org.sisioh.sip.core.Separators
import org.sisioh.sip.message.address.impl.DefaultGenericURI

class RequestLineSpec extends Specification {
  "SIPRequestLine" should {
    "デフォルトのバージョンを取得できる" in {
      val target = RequestLine(DefaultGenericURI("test:test"), Some("INVITE"))
      target.sipVersion must_== Some(SIPConstants.SIP_VERSION_STRING)
      target.versionMajor must_== Some("2")
      target.versionMinor must_== Some("0")
    }
    "指定したバージョンが取得できる" in {
      "スラッシュがある場合" in {
        val sipVersion = "SIP/1.1"
        val target = RequestLine(DefaultGenericURI("test:test"), Some("INVITE"), Some(sipVersion))
        target.sipVersion must_== Some(sipVersion)
        target.versionMajor must_== Some("1")
        target.versionMinor must_== Some("1")
      }
      "スラッシュがない場合" in {
        val sipVersion = "1.1"
        val target = RequestLine(DefaultGenericURI("test:test"), Some("INVITE"), Some(sipVersion))
        target.sipVersion must_== Some(sipVersion)
        target.versionMajor must_== Some("1")
        target.versionMinor must_== Some("1")
      }
    }
    "不正なバージョンの場合" in {
      "スラッシュがある場合" in {
        val sipVersion = "SIP/11"
        val target = RequestLine(DefaultGenericURI("test:test"), Some("INVITE"), Some(sipVersion))
        target.sipVersion must_== Some(sipVersion)
        target.versionMajor must_== None
        target.versionMinor must_== None
      }
      "スラッシュがない場合" in {
        val sipVersion = "11"
        val target = RequestLine(DefaultGenericURI("test:test"), Some("INVITE"), Some(sipVersion))
        target.sipVersion must_== Some(sipVersion)
        target.versionMajor must_== None
        target.versionMinor must_== None
      }
    }
    "エンコード結果が取得できること" in {
      val target = RequestLine(DefaultGenericURI("test:testurl"), Some("INVITE"))
      target.encode() must_== """INVITE test:testurl SIP/2.0""" + Separators.NEWLINE
      target.encodeByJson() must_== """{"uri":{"uriString":"test:testurl"},"method":"INVITE","sipVersion":"SIP/2.0"}"""
    }
  }
}
