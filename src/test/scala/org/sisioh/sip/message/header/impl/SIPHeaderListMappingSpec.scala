package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.{HostPort, Host}
import org.sisioh.sip.message.header.{impl, Protocol}

class SIPHeaderListMappingSpec extends Specification {

  "SIPHeaderListMapping" should {
    val via = new Via(HostPort(Host("localhost"), None), Protocol("SIP", "1.0", "TCP"))
    "ViaクラスはViaListを持っている" in {
      SIPHeaderListMapping.hasList(classOf[Via]) must_== true
    }
    "ViaインスタンスはViaListを持っている" in {
      SIPHeaderListMapping.hasList(via) must_== true
    }
    "ViaクラスからViaListクラスを取得する" in {
      SIPHeaderListMapping.getListClass(classOf[Via]) must beSome(classOf[ViaList])
    }
    "ViaオブジェクトからViaListオブジェクトを取得する" in {
      SIPHeaderListMapping.getList(via) must_== Some(ViaList())
    }
  }

}
