package org.sisioh.sip.message.address.impl

/*
 * Copyright 2012 Sisioh Project and others. (http://www.sisioh.org/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */


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

  def createSipURI(user: Option[String], host: String): SipUri = {
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
