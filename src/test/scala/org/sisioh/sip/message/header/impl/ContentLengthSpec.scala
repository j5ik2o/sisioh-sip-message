package org.sisioh.sip.message.header.impl

import org.specs2.mutable.Specification

class ContentLengthSpec extends Specification {
  "ContentLength" should {
    val target = ContentLength(100)
    target.contentLength must_== 100
  }
}
