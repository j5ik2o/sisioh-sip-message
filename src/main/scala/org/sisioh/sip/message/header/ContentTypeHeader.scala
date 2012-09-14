package org.sisioh.sip.message.header

trait ContentTypeHeader extends MediaType with Parameters with Header

object ContentTypeHeader {
  val NAME = "Content-Type"
}