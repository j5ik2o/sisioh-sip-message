package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util.{Host, HostPort}
import org.sisioh.sip.message.header.Protocol
import org.specs2.mutable.Specification
import org.sisioh.sip.core.Separators

class ViaListSpec extends Specification {
  "ViaList" should {
    val via = Via(HostPort(Host("localhost"), None), Protocol("SIP", "2.0", "TCP"))
    val target = ViaList(List(via))

    "要素が１つ存在すること" in {
      target.toList must have size(1)
      target.toList must_== List(via)
    }

    val via2 = Via(HostPort(Host("localhost"), None), Protocol("SIP", "2.1", "TCP"))

    "要素を追加できること" in {
      val newTarget = target.addLast(via2)
      newTarget.toList must have size(2)
      newTarget.toList must_== List(via, via2)
   }

    "要素を削除できること" in {
      val newTarget = target.remove(via2)
      newTarget.toList must have size(1)
      newTarget.toList must_== List(via)
    }

    "ただしいエンコード結果を取得できる" in {
      ViaList(List(via)).encode() must_== """Via: SIP/2.0/TCP localhost""" + Separators.NEWLINE
      ViaList(List(via, via2)).encode() must_== """Via: SIP/2.0/TCP localhost;SIP/2.1/TCP localhost""" + Separators.NEWLINE
      ViaList(List(via)).encodeByJson() must_== """{"headerNamne":"Via","parameters":[{"headerName":"Via","sentBy":{"host":"localhost"},"sentProtocol":{"protocolName":"SIP","protocolVersion":"2.0","transport":"TCP"},"parameters":{}}]}"""
      ViaList(List(via, via2)).encodeByJson() must_== """{"headerNamne":"Via","parameters":[{"headerName":"Via","sentBy":{"host":"localhost"},"sentProtocol":{"protocolName":"SIP","protocolVersion":"2.0","transport":"TCP"},"parameters":{}},{"headerName":"Via","sentBy":{"host":"localhost"},"sentProtocol":{"protocolName":"SIP","protocolVersion":"2.1","transport":"TCP"},"parameters":{}}]}"""
    }

  }
}
