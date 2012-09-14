package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{Address, URI}

object AddressType extends Enumeration {
  val NAME, ADDRESS, WILD_CARD = Value
}

case class DefaultAddress
(uri: URI,
 displayName: Option[String] = None,
 isWildcard: Boolean = false) extends Address {

  if (displayName.isDefined) {

  }





}
