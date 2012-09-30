package org.sisioh.sip.message.header.impl

import org.sisioh.sip.core.Separators
import org.specs2.mutable.Specification

class UserAgentSpec extends Specification {
  "UserAgent" should {
    val target = UserAgent(List(Product("PRODUCT1")))
    "正しい属性が取得できること" in {
      target.serverVals must_== List(Product("PRODUCT1"))
    }
    "プロダクトを追加できること" in {
      target.addProduct(Product("PRODUCT2")).serverVals must_== List(Product("PRODUCT2"), Product("PRODUCT1"))
    }
    "エンコード結果を取得できること" in {
      target.encode() must_== """User-Agent: PRODUCT1""" + Separators.NEWLINE
      target.encodeByJson() must_== """{"headerName":"User-Agent","serverVals":[{"type":"product","value":"PRODUCT1"}]}"""
      target.addProduct(Product("PRODUCT2")).encode() must_== """User-Agent: PRODUCT2 PRODUCT1""" + Separators.NEWLINE
      target.addProduct(Product("PRODUCT2")).encodeByJson() must_== """{"headerName":"User-Agent","serverVals":[{"type":"product","value":"PRODUCT2"},{"type":"product","value":"PRODUCT1"}]}"""
    }
  }
}
