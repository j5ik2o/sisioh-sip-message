package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class UserAgentDecoderSpec extends Specification {
  "UserAgentDecoder" should {
    "productVersion" in {
      UserAgentDecoder.decodeTarget("1.0", UserAgentDecoder.productVersion) must_== "1.0"
    }
    "product" in {
      UserAgentDecoder.decodeTarget("ABC/1.0", UserAgentDecoder.product) must_== Product.from("ABC/1.0")
      UserAgentDecoder.decodeTarget("ABC", UserAgentDecoder.product) must_== Product("ABC")
    }
    "serverVal" in {
      UserAgentDecoder.decodeTarget("ABC/1.0", UserAgentDecoder.serverVal) must_== Product.from("ABC/1.0")
      UserAgentDecoder.decodeTarget("ABC", UserAgentDecoder.serverVal) must_== Product("ABC")
      UserAgentDecoder.decodeTarget("(comment)", UserAgentDecoder.serverVal) must_== Comment("(comment)")
    }
    "USER_AGENT" in {
      UserAgentDecoder.decodeTarget("User-Agent: ABC/1.0", UserAgentDecoder.USER_AGENT) must_== UserAgent(List(Product.from("ABC/1.0")))
      UserAgentDecoder.decodeTarget("User-Agent: ABC", UserAgentDecoder.USER_AGENT) must_== UserAgent(List(Product("ABC")))
      UserAgentDecoder.decodeTarget("User-Agent: (comment)", UserAgentDecoder.USER_AGENT) must_== UserAgent(List(Comment("(comment)")))
    }
    "USER_AGENTWithCrLfOpt" in {
      UserAgentDecoder.decodeTarget("User-Agent: ABC/1.0\r\n", UserAgentDecoder.USER_AGENTWithCrLfOpt) must_== UserAgent(List(Product.from("ABC/1.0")))
      UserAgentDecoder.decodeTarget("User-Agent: ABC\r\n", UserAgentDecoder.USER_AGENTWithCrLfOpt) must_== UserAgent(List(Product("ABC")))
      UserAgentDecoder.decodeTarget("User-Agent: (comment)\r\n", UserAgentDecoder.USER_AGENTWithCrLfOpt) must_== UserAgent(List(Comment("(comment)")))
    }

    "可逆的にデコードできること" in {
      "プロダクトが1つの場合" in {
        "プロダクト名とバージョンがある場合" in {
          val source = UserAgent(List(Product.from("ABC/1.0")))
          val dest = UserAgentDecoder.decode(source.encode())
          dest must_== source
        }
        "プロダクト名がある場合" in {
          val source = UserAgent(List(Product("ABC")))
          val dest = UserAgentDecoder.decode(source.encode())
          dest must_== source
        }
      }
      "プロダクトが複数の場合" in {
        "プロダクト名とバージョンがある場合" in {
          val source = UserAgent(List(Product.from("PRODUCT1/1.0"), Product.from("PRODUCT2/1.0")))
          val dest = UserAgentDecoder.decode(source.encode())
          dest must_== source
        }
        "プロダクト名がある場合" in {
          val source = UserAgent(List(Product("PRODUCT1"), Product("PRODUCT2")))
          val dest = UserAgentDecoder.decode(source.encode())
          dest must_== source
        }
      }
    }
  }
}
