package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.NameValuePair
import org.sisioh.sip.message.address.ParameterNames

class TelephoneNumberDecoderSpec extends Specification {
  "TelephoneNumberDecoder" should {
    "isdnSubAddress" in {
      TelephoneNumberDecoder.decodeTarget(";isub=09012345678", TelephoneNumberDecoder.isdnSubAddress) must_== NameValuePair(Some(ParameterNames.ISUB), Some("09012345678"))
    }
    "basePhoneNumber" in {
      TelephoneNumberDecoder.decodeTarget("09012345678", TelephoneNumberDecoder.basePhoneNumber) must_== "09012345678"
    }
    "postDial" in {
      TelephoneNumberDecoder.decodeTarget(";postd=09012345678", TelephoneNumberDecoder.postDial) must_== NameValuePair(Some(ParameterNames.POSTDIAL), Some("09012345678"))
    }
    "localNetworkPrefix" in {
      TelephoneNumberDecoder.decodeTarget("4321", TelephoneNumberDecoder.localNetworkPrefix) must_== "4321"
    }
    "globalNetworkPrefix" in {
      TelephoneNumberDecoder.decodeTarget("+09012345678", TelephoneNumberDecoder.globalNetworkPrefix) must_== "+09012345678"
    }
    "networkPrefix" in {
      TelephoneNumberDecoder.decodeTarget("+09012345678", TelephoneNumberDecoder.networkPrefix) must_== "+09012345678"
      TelephoneNumberDecoder.decodeTarget("4321", TelephoneNumberDecoder.networkPrefix) must_== "4321"
    }
    "privatePrefix" in {
      val privatePrefix = "!!"
      TelephoneNumberDecoder.decodeTarget(privatePrefix, TelephoneNumberDecoder.privatePrefix) must_== privatePrefix
    }
    "phoneContextIdent" in {
      val privatePrefix = "!!"
      TelephoneNumberDecoder.decodeTarget(privatePrefix, TelephoneNumberDecoder.phoneContextIdent) must_== privatePrefix
      TelephoneNumberDecoder.decodeTarget("+09012345678", TelephoneNumberDecoder.phoneContextIdent) must_== "+09012345678"
      TelephoneNumberDecoder.decodeTarget("4321", TelephoneNumberDecoder.phoneContextIdent) must_== "4321"
    }
    "areaSpecifier" in {
      val areaSpecifier = ";phone-context=!!"
      TelephoneNumberDecoder.decodeTarget(areaSpecifier, TelephoneNumberDecoder.areaSpecifier) must_== NameValuePair(Some("phone-context"), Some("!!"))
    }
    "providerHostname" in {
      val providerHostname = "localhost"
      TelephoneNumberDecoder.decodeTarget(providerHostname, TelephoneNumberDecoder.providerHostname) must_== providerHostname
    }
    "serviceProvider" in {
      val serviceProvider = ";tsp=localhost"
      TelephoneNumberDecoder.decodeTarget(serviceProvider, TelephoneNumberDecoder.serviceProvider) must_== NameValuePair(Some("tsp"), Some("localhost"))
    }
    "futureExtension" in {
      val futureExtension = ";tsp=localhost"
      TelephoneNumberDecoder.decodeTarget(futureExtension, TelephoneNumberDecoder.futureExtension) must_== NameValuePair(Some("tsp"), Some("localhost"))
    }
    "globalPhoneNumber" in {
      val globalPhoneNumber = "+09012345678"
      TelephoneNumberDecoder.decodeTarget(globalPhoneNumber, TelephoneNumberDecoder.globalPhoneNumber) must_== TelephoneNumber("09012345678")
    }
    "globalPhoneNumber&futureExtension" in {
      val globalPhoneNumber = "+09012345678;tsp=localhost"
      TelephoneNumberDecoder.decodeTarget(globalPhoneNumber, TelephoneNumberDecoder.globalPhoneNumber) must_==
        TelephoneNumber("09012345678").withParameter("tsp", "localhost")
    }

  }
}
