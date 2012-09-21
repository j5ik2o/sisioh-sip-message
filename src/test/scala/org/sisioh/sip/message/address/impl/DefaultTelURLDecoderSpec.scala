package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification

class DefaultTelURLDecoderSpec  extends Specification {

  "DefaultTelURLDecoder" should {
    "globalPhoneNumber&futureExtension" in {
      val phoneNumber = "tel:+09012345678;tsp=localhost"
      DefaultTelURLDecoder().decode(phoneNumber) must_==
        DefaultTelURL(TelephoneNumber("09012345678").withParameter("tsp", "localhost"))
    }
    "globalPhoneNumber&phone-context" in {
      val phoneNumber = "tel:+09012345678;phone-context=localhost"
      DefaultTelURLDecoder().decode(phoneNumber) must_==
        DefaultTelURL(TelephoneNumber("09012345678").withParameter("phone-context", "localhost"))
    }
  }

}
