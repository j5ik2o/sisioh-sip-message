package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{URI, AddressFactory}

class DefaultAddressFactory extends AddressFactory with AddressFactoryEx {

  def createURI(uri: String) = null

  def createSipURI(user: String, host: String) = null

  def createSipURI(sipUri: URI) = null

  def createTelURL(phoneNumber: String) = null

  def createAddress(address: String) = {
//    if (address == "*"){
//      val uri = SipUri(user = Some("*"), password = None, host = None, port = None)
//      DefaultAddress.fromURI(uri)
//    } else {
//
//    }
    null
  }

  def createAddress(uri: URI) =
    createAddress(uri)

  def createAddress(uri: URI, displayName: Option[String]) = null

  def createSipURI(sipUri: String) = null
}
