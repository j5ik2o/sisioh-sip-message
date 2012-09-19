package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ExpiresHeader

case class Expires(expires: Int) extends ExpiresHeader {
  require(expires > 0)
}
