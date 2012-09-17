package org.sisioh.sip.util

import org.specs2.mutable.Specification

class NameValuePairListSpec extends Specification {

  "NameValuePairList" should {
    var target = NameValuePairList()
    target = target.add("name", "kato")
    target = target.add("age", 40)
    println(target.encode())
    true must_== true
  }

}
