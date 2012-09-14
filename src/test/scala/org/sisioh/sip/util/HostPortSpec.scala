package org.sisioh.sip.util

import org.specs2.mutable.Specification

class HostPortSpec extends Specification {
  "localhost:8080" should {
    val host = Host("localhost")
    val port = Some(8080)
    val hostPort = HostPort(host,port)
    "localhostが取得できる" in {
      hostPort.host must_== host
    }
    "8080が取得できる" in {
      hostPort.port must_== port
    }
    "エンコード結果としてlocalhost:8080が取得できる" in {
      hostPort.encode must_== "localhost:8080"
    }
  }
}
