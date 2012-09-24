package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class ServerSpec extends Specification {
  "Server" should {
    val target = Server(List(Product("PRODUCT1")))
    "正しい属性が取得できること" in {
      target.serverVals must_== List(Product("PRODUCT1"))
    }
    "プロダクトを追加できること" in {
      target.addProduct(Product("PRODUCT2")).serverVals must_== List(Product("PRODUCT2"), Product("PRODUCT1"))
    }
  }
}
