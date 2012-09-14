package org.sisioh.sip.message.header

import java.util

trait ContentLanguageHeader extends Header {
  val contentLanguage: util.Locale
}

object ContentLanguageHeader {
  val NAME = "Content-Language"
}
