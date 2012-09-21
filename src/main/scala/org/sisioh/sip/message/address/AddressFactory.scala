package org.sisioh.sip.message.address

trait AddressFactory {

  def createURI(uri: String): URI

  def createSipURI(user: Option[String], host: String): SipURI

  def createSipURI(uri: String): SipURI

  def createTelURL(phoneNumber: String): TelURL

  def createAddress(address: String): Address

  def createAddress(uri: URI): Address

  def createAddress(uri: URI, displayName: Option[String]): Address

}
