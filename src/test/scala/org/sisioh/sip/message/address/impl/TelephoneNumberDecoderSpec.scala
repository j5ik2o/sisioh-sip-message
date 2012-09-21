package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.NameValuePair

class TelephoneNumberDecoderSpec extends Specification {
  "TelephoneNumberDecoder" should {
    val target = TelephoneNumberDecoder()
    "isdnSubAddress" in {
      target.decodeTarget(";isub=09012345678", target.isdnSubAddress) must_== NameValuePair(Some(ParameterNames.ISUB), Some("09012345678"))
    }
    "basePhoneNumber" in {
      target.decodeTarget("09012345678", target.basePhoneNumber) must_== "09012345678"
    }
    "postDial" in {
      target.decodeTarget(";postd=09012345678", target.postDial) must_== NameValuePair(Some(ParameterNames.POSTDIAL), Some("09012345678"))
    }
    "localNetworkPrefix" in {
      target.decodeTarget("4321", target.localNetworkPrefix) must_== "4321"
    }
    "globalNetworkPrefix" in {
      target.decodeTarget("+09012345678", target.globalNetworkPrefix) must_== "+09012345678"
    }
    "networkPrefix" in {
      target.decodeTarget("+09012345678", target.networkPrefix) must_== "+09012345678"
      target.decodeTarget("4321", target.networkPrefix) must_== "4321"
    }
    "privatePrefix" in {
      val privatePrefix = "!!"
      target.decodeTarget(privatePrefix, target.privatePrefix) must_== privatePrefix
    }
    "phoneContextIdent" in {
      val privatePrefix = "!!"
      target.decodeTarget(privatePrefix, target.phoneContextIdent) must_== privatePrefix
      target.decodeTarget("+09012345678", target.phoneContextIdent) must_== "+09012345678"
      target.decodeTarget("4321", target.phoneContextIdent) must_== "4321"
    }
    "areaSpecifier" in {
      val areaSpecifier = ";phone-context=!!"
      target.decodeTarget(areaSpecifier, target.areaSpecifier) must_== NameValuePair(Some("phone-context"), Some("!!"))
    }
    "providerHostname" in {
      val providerHostname = "localhost"
      target.decodeTarget(providerHostname, target.providerHostname) must_== providerHostname
    }
    "serviceProvider" in {
      val serviceProvider = ";tsp=localhost"
      target.decodeTarget(serviceProvider, target.serviceProvider) must_== NameValuePair(Some("tsp"), Some("localhost"))
    }
    "futureExtension" in {
      val futureExtension = ";tsp=localhost"
      target.decodeTarget(futureExtension, target.futureExtension) must_== NameValuePair(Some("tsp"), Some("localhost"))
    }
    "globalPhoneNumber" in {
      val globalPhoneNumber = "+09012345678"
      target.decodeTarget(globalPhoneNumber, target.globalPhoneNumber) must_== TelephoneNumber("09012345678")
    }
    "globalPhoneNumber&futureExtension" in {
      val globalPhoneNumber = "+09012345678;tsp=localhost"
      target.decodeTarget(globalPhoneNumber, target.globalPhoneNumber) must_==
        TelephoneNumber("09012345678").withParameter("tsp", "localhost")
    }

  }
}
