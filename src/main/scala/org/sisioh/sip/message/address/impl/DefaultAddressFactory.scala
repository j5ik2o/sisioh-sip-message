package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.{Address, SipURI, URI, AddressFactory}

class DefaultAddressFactory extends AddressFactory with AddressFactoryEx {

  def createURI(uri: String): URI = {
    val splits = uri.split(":")
    require(splits.length > 1)
    val scheme = splits(0)
    if (scheme.equalsIgnoreCase("sip") || scheme.equalsIgnoreCase("sips")) {
      createSipURI(uri)
    } else if (scheme.equalsIgnoreCase("tel")) {
      createTelURL(uri)
    } else {
      DefaultGenericURI(uri)
    }
  }

  def createSipURI(user: Option[String], host: String): SipURI = {
    val uriString = new StringBuilder("sip:")
    if (user.isDefined) {
      uriString.append(user.get)
      uriString.append("@")
    }
    val formatHost = if (host.indexOf(':') != host.lastIndexOf(':') && host.trim().charAt(0) != '[')
      '[' + host + ']'
    else
      host
    uriString.append(formatHost)
    createSipURI(uriString.result())
  }

  def createTelURL(phoneNumber: String) = {
    DefaultTelURL.decode(phoneNumber)
  }

  def createAddress(address: String): DefaultAddress = {
    if (address == "*") {
      val uri = SipUri.fromUserAndHost(user = Some("*"), password = None, host = None, port = None)
      DefaultAddress.fromURI(uri, None, Some(AddressType.WILD_CARD))
    } else {
      DefaultAddress.decode(address)
    }
  }

  def createAddress(uri: URI) =
    createAddress(uri, None)

  def createAddress(uri: URI, displayName: Option[String]): DefaultAddress = {
    DefaultAddress(uri.asInstanceOf[GenericURI], displayName)
  }

  def createSipURI(sipUri: String): SipUri = SipUri.decode(sipUri)
}
