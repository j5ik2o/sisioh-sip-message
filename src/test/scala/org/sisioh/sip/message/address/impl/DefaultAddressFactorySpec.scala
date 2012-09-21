package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.message.address.NetObject

class DefaultAddressFactorySpec extends Specification {

  "DefaultAddressFactory" should {
    val target = new DefaultAddressFactory
    "SipURIを生成できること" in {
      "SIPURIから生成する場合" in {
        val sipuri = target.createSipURI("sip:hoge@localhost")
        sipuri must_== SipUri.fromUserAndHost(Some("hoge"), None, Some("localhost"), None)
      }
      "ユーザ名とホストから生成する場合" in {
        val sipuri = target.createSipURI(Some("hoge"), "localhost")
        sipuri must_== SipUri.fromUserAndHost(Some("hoge"), None, Some("localhost"), None)
      }
    }
    "URIを生成できること" in {
      "SIPURIの場合" in {
        val uri = target.createURI("sip:hoge@localhost")
        uri must_== SipUri.fromUserAndHost(Some("hoge"), None, Some("localhost"), None)
      }
      "SIPSURIの場合" in {
        val uri = target.createURI("sips:hoge@localhost")
        uri must_== SipUri.fromUserAndHost(Some("hoge"), None, Some("localhost"), None, NetObject.SIPS)
      }
      "電話番号の場合" in {
        val uri = target.createURI("tel:+09012345678")
        uri must_== DefaultTelURL(TelephoneNumber("09012345678"))
      }
      "汎用的なURIの場合" in {
        val uri = target.createURI("test:testid")
        uri must_== DefaultGenericURI("test:testid")
      }
    }
    "Addressを生成できること" in {
      val uri = target.createURI("test:testid")
      "URIのみの場合" in {
        val address = target.createAddress(uri)
        address must_== DefaultAddress(uri.asInstanceOf[GenericURI])
      }
      "URIと表示名の場合" in {
        val address = target.createAddress(uri, Some("kato"))
        address must_== DefaultAddress(uri.asInstanceOf[GenericURI], Some("kato"))
      }
      "文字列のURIのみの場合" in {
        val sipuri = target.createURI("sip:testid@localhost")
        val sourceAddress = DefaultAddress(sipuri.asInstanceOf[GenericURI], Some("kato"))
        val encode = sourceAddress.encode()
        println(encode)
        val destAddress = target.createAddress(encode)
        destAddress must_== sourceAddress
      }
    }
    "TelURLを生成できること" in {
      val phoneNumber = target.createTelURL("tel:+09012345678")
      phoneNumber must_== DefaultTelURL(TelephoneNumber("09012345678"))
    }
  }

}
