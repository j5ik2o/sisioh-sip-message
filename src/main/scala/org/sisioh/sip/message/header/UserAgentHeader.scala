package org.sisioh.sip.message.header

import impl.ServerVal

trait UserAgentHeader extends Header {
  val serverVals: List[ServerVal]
}

object UserAgentHeader {
  val NAME = "User-Agent"
}
