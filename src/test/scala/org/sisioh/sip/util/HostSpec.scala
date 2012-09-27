package org.sisioh.sip.util

import org.specs2.mutable.Specification
import java.net.InetAddress

class HostSpec extends Specification {

  "localhost" should {
    val hostName = "localhost"
    val host = Host(hostName)
    "be hostName" in {
      host.isHostName must_== true
      host.isIpAddress must_== false
      host.hostName.isDefined must_== true
      host.ipAddress.isDefined must_== false
      host.inetAddress must_== InetAddress.getByName(hostName)
      host.resolvedIpAddress must_== "127.0.0.1"
      host.toString must_== "localhost"
      host.encode() must_== "localhost"
      host.encodeByJson() must_== """{"hostNameOrIpAddress":"localhost","addressType":0}"""
    }
  }

  "127.0.0.1" should {
    val ipAddress = "127.0.0.1"
    val host = new Host(ipAddress)
    "be ip address" in {
      host.isHostName must_== false
      host.isIpAddress must_== true
      host.hostName.isDefined must_== false
      host.ipAddress.isDefined must_== true
      host.inetAddress must_== InetAddress.getByName(ipAddress)
      host.resolvedIpAddress must_== "127.0.0.1"
      host.toString must_== "127.0.0.1"
      host.encode() must_== "127.0.0.1"
      host.encodeByJson() must_== """{"hostNameOrIpAddress":"127.0.0.1","addressType":1}"""
    }
  }

}
