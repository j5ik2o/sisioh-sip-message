package org.sisioh.sip.util

import org.specs2.mutable.Specification

class UtilsSpec extends Specification {
  "Utils" should {
    "signature" in {
      val regex = """\d+""".r
      regex.findFirstMatchIn(Utils.signature).isDefined must_== true
    }
    "toHexString" in {
      Utils.toHexString("1".getBytes) must_== "31"
    }
    "getQuotedString" in {
      Utils.getQuotedString("ABC") must_== "\"ABC\""
    }
    "reduceString" in {
      Utils.reduceString("A B\tC") must_== "abc"
    }
    "generateCallIdentifier" in {
      Utils.generateCallIdentifier("test@test") must endWith("test@test")
    }
    "generateBranchId" in {
      Utils.generateBranchId() must startWith("z9hG4bK")
    }
  }

}
