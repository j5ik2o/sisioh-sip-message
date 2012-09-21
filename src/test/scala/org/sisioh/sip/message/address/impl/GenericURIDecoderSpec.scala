package org.sisioh.sip.message.address.impl

import org.specs2.mutable.Specification
import org.sisioh.sip.util.ParseException

class GenericURIDecoderSpec extends Specification {

  "GenericURIDecoder" should {
    val source = DefaultGenericURI("test:abc")
    val encodeObject = source.encode()
    "可逆的にデコードできること" in {
      val dest = DefaultGenericURIDecoder().decode(encodeObject)
      dest must_== source
    }
    "デコードできること" in {
      DefaultGenericURIDecoder().decode("test://kato@localhost/hoge") must haveClass[DefaultGenericURI]
      DefaultGenericURIDecoder().decode("test://kato:password@localhost/hoge") must haveClass[DefaultGenericURI]
    }
    "不正なエンコードデータはデコードに失敗すること" in {
      DefaultGenericURIDecoder().decode("test") must throwA[ParseException]
      DefaultGenericURIDecoder().decode("test:\\") must throwA[ParseException]
    }
  }

}
