package org.sisioh.sip.util

import org.specs2.mutable.Specification

class NameValuePairSpec extends Specification {

  "NameValuePair" should {
    val name = "name"
    val value = "value"
    "nameとvalueを指定した場合" in {
      val nameValue = NameValuePair(Some(name), Some(value))
      "name=value形式であること" in {
        nameValue.encode() must_== """name=value"""
        nameValue.encodeByJson() must_== """{"name":"name","type":"string","value":"value"}"""
      }
    }
    "nameとvalue、フラグ指定した場合" in {
      val nameValue = NameValuePair(Some(name), Some(true))
      "nameであること" in {
        nameValue.encode() must_== """name"""
        nameValue.encodeByJson() must_== """{"name":"name","type":"boolean","value":true}"""
      }
      val nameValue2 = NameValuePair(Some(name), Some(false))
      "空文字列であること" in {
        nameValue2.encode() must_== ""
        nameValue2.encodeByJson() must_== """{"name":"name","type":"boolean","value":false}"""
      }
    }
    "nameとvalueと独自のセパレータを指定した場合" in {
      val nameValue = NameValuePair(Some(name), Some(value), separator = ":=")
      "name:=value形式であること" in {
        nameValue.encode() must_== """name:=value"""
        nameValue.encodeByJson() must_== """{"name":"name","type":"string","value":"value"}"""
      }
    }

  }


}
