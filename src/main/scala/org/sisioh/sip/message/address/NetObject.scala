package org.sisioh.sip.message.address

import org.sisioh.sip.core.PackageNames

trait NetObject {

}

object NetObject {

  val CORE_PACKAGE = PackageNames.CORE_PACKAGE;
  val NET_PACKAGE = PackageNames.NET_PACKAGE;
  val PARSER_PACKAGE = PackageNames.PARSER_PACKAGE;
  val UDP = "udp";
  val TCP = "tcp";
  val TRANSPORT = "transport";
  val METHOD = "method";
  val USER = "user";
  val PHONE = "phone";
  val MADDR = "maddr";
  val TTL = "ttl";
  val LR = "lr";
  val SIP = "sip";
  val SIPS = "sips";
  val TLS = "tls"
}
