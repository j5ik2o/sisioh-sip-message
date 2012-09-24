package org.sisioh.sip.message.impl

import org.sisioh.sip.message.header.{Header, ContentDispositionHeader, ContentTypeHeader}

trait Conent {
  val content: Any
  val contentTypeHeader: Option[ContentTypeHeader]
  val contentDispositionHeader: Option[ContentDispositionHeader]
  def extensionHeaders: Iterator[Header]
}
