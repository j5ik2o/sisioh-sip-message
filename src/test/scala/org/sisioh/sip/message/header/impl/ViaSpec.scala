package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util.{Host, HostPort}
import org.sisioh.sip.message.header.Protocol
import org.specs2.mutable.Specification
import org.sisioh.sip.core.Separators
import org.sisioh.sip.message.header.ParameterNames

class ViaSpec extends Specification {
  "Via" should {
    val via = Via(HostPort(Host("localhost"), None), Protocol("SIP", "2.0", "TCP"))

    "属性が取得できること" in {
      via.host must_== "localhost"
      via.port must_== None
      via.protocol must_== "SIP"
      via.protocolVersion must_== "2.0"
      via.transport must_== "TCP"
    }

    "パラメータが設定でき読めること" in {
      via.withParameter("test", "abc").getParameter("test") must_== Some("abc")
      via.withBranch("test").branch must_== Some("test")
      via.withBranch("test").getParameter(ParameterNames.BRANCH) must_== Some("test")
      via.withMAddr("0.0.0.0").maddr must_== Some("0.0.0.0")
      via.withMAddr("0.0.0.0").getParameter(ParameterNames.MADDR) must_== Some("0.0.0.0")
    }

    "正しいエンコード結果が得られること" in {
      via.encode() must_== """Via: SIP/2.0/TCP localhost""" + Separators.NEWLINE
      via.encodeByJson() must_== """{"headerName":"Via","sentBy":{"host":{"hostNameOrIpAddress":"localhost","addressType":0}},"sentProtocol":{"protocolName":"SIP","protocolVersion":"2.0","transport":"TCP"},"parameters":{"separator":";","values":{}}}"""
      via.withBranch("test").encode() must_== """Via: SIP/2.0/TCP localhost;branch=test""" + Separators.NEWLINE
      via.withBranch("test").encodeByJson() must_== """{"headerName":"Via","sentBy":{"host":{"hostNameOrIpAddress":"localhost","addressType":0}},"sentProtocol":{"protocolName":"SIP","protocolVersion":"2.0","transport":"TCP"},"parameters":{"separator":";","values":{"branch":"test"}}}"""
      via.withParameter("test", "abc").encode() must_== """Via: SIP/2.0/TCP localhost;test=abc""" + Separators.NEWLINE
      via.withParameter("test", "abc").encodeByJson() must_== """{"headerName":"Via","sentBy":{"host":{"hostNameOrIpAddress":"localhost","addressType":0}},"sentProtocol":{"protocolName":"SIP","protocolVersion":"2.0","transport":"TCP"},"parameters":{"separator":";","values":{"test":"abc"}}}"""
    }

  }
}
