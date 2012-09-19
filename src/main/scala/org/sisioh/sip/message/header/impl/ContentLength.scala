package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ContentLengthHeader

case class ContentLength(contentLength: Int) extends ContentLengthHeader {
  require(contentLength > 0)
  val name = ContentLengthHeader.NAME
}
