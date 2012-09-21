package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification


class DefaultTelURLSpec extends Specification {
  "DefaultTelURL" should {
    "電話番号のみの場合" in {
      val target = DefaultTelURL(TelephoneNumber("09012345678"))
      "グローバルな電話番号である" in {
        target.isGlobal must_== true
      }
      "ポストダイアルではない" in {
        target.hasPostDial must_== false
      }
      "ISDNサブアドレスではない" in {
        target.hasIsdnSubAddress must_== false
      }
      "パラメータのサイズは0である" in {
        target.params must size(0)
      }
      "グローバルな電話番号としてのエンコード結果が得られる" in {
        target.encode() must_== "tel:+09012345678"
        target.encodeByJson() must_== """{"scheme":"tel","isGlobal":true,"phoneNumber":"09012345678","parameters":{}}"""
      }
    }
    "電話番号とパラメータを指定した場合" in {
      val target = DefaultTelURL(TelephoneNumber("09012345678")).withParameter("PARAM1", "param1")
      "ポストダイアルではない" in {
        target.hasPostDial must_== false
      }
      "ISDNサブアドレスではない" in {
        target.hasIsdnSubAddress must_== false
      }
      "パラメータのサイズは1である" in {
        target.params must size(1)
      }
      "グローバルな電話番号である" in {
        target.isGlobal must_== true
      }
      "グローバルなパラメータ付き電話番号としてのエンコード結果が得られる" in {
        target.encode() must_== "tel:+09012345678;PARAM1=param1"
        target.encodeByJson() must_== """{"scheme":"tel","isGlobal":true,"phoneNumber":"09012345678","parameters":{"PARAM1":"param1"}}"""
      }
    }

    "プライベートな電話番号を指定した場合" in {
      val target = DefaultTelURL(TelephoneNumber("4321", false))
      "プライベートな電話番号としてのエンコード結果が得られる" in {
        target.encode() must_== "tel:4321"
        target.encodeByJson() must_== """{"scheme":"tel","isGlobal":false,"phoneNumber":"4321","parameters":{}}"""
      }
    }
    "ポストダイアルを指定した場合" in {
      val target = DefaultTelURL(TelephoneNumber("4321",false)).withPostDial("abc")
      "ポストダイアル付き電話番号としてのエンコード結果が得られる" in {
        target.encode() must_== "tel:4321;postd=abc"
        target.encodeByJson() must_== """{"scheme":"tel","isGlobal":false,"phoneNumber":"4321","parameters":{"postd":"abc"}}"""
      }
    }
    "ISDNサブアドレスを指定した場合" in {
      val target = DefaultTelURL(TelephoneNumber("4321",false)).withIsdnSubaddress("0123")
      "ISDNサブアドレス付き電話番号としてのエンコード結果が得られる" in {
        target.encode() must_== "tel:4321;isub=0123"
        target.encodeByJson() must_== """{"scheme":"tel","isGlobal":false,"phoneNumber":"4321","parameters":{"isub":"0123"}}"""
      }
    }
  }
}
