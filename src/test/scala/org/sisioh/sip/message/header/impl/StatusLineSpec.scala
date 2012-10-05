package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.header.SIPConstants
import org.sisioh.sip.message.impl.SIPResponse
import org.sisioh.sip.core.Separators
import org.sisioh.sip.message.StatusCode

class StatusLineSpec extends Specification {
  "StatusLine" should {
    "デフォルトのバージョンを取得できる" in {
      val target = StatusLine(StatusCode(200))
      target.statusCode must_== StatusCode.OK
      target.reasonPhrase must_== SIPResponse.getReasonPhrase(200)
      target.sipVersion must_== Some(SIPConstants.SIP_VERSION_STRING)
      target.versionMajor must_== Some("2")
      target.versionMinor must_== Some("0")
    }
    "指定したバージョンが取得できる" in {
      "スラッシュがある場合" in {
        val sipVersion = "SIP/1.1"
        val target = StatusLine(StatusCode(200), None, Some(sipVersion))
        target.sipVersion must_== Some(sipVersion)
        target.versionMajor must_== Some("1")
        target.versionMinor must_== Some("1")
      }
      "スラッシュがない場合" in {
        val sipVersion = "1.1"
        val target = StatusLine(StatusCode(200), None, Some(sipVersion))
        target.sipVersion must_== Some(sipVersion)
        target.versionMajor must_== Some("1")
        target.versionMinor must_== Some("1")
      }
    }
    "不正なバージョンの場合" in {
      "スラッシュがある場合" in {
        val sipVersion = "SIP/11"
        val target = StatusLine(StatusCode(200), None, Some(sipVersion))
        target.sipVersion must_== Some(sipVersion)
        target.versionMajor must_== None
        target.versionMinor must_== None
      }
      "スラッシュがない場合" in {
        val sipVersion = "11"
        val target = StatusLine(StatusCode(200), None, Some(sipVersion))
        target.sipVersion must_== Some(sipVersion)
        target.versionMajor must_== None
        target.versionMinor must_== None
      }
    }
    "エンコード結果が取得できること" in {
      val target = StatusLine(StatusCode(200))
      target.encode() must_== """SIP/2.0 200 OK""" + Separators.NEWLINE
      target.encodeByJson() must_== """{"statusCode":200,"reasonPhrase":"OK","sipVersion":"SIP/2.0"}"""
    }
  }
}
