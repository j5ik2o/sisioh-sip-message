package org.sisioh.sip.util

import org.specs2.mutable.Specification

class NameValuePairListSpec extends Specification {

  "NameValuePairList" should {
    var target = NameValuePairList()
    target = target.add("name", "kato")
    target = target.add("age", 40)
    target.encode() must_== "name=kato;age=40"
    target.encodeByJson() must_== """{"name":"kato","age":"40"}"""
  }

}
