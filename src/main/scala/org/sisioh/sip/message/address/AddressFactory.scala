package org.sisioh.sip.message.address

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


trait AddressFactory {

  def createURI(uri: String): URI

  def createSipURI(user: Option[String], host: String): SipURI

  def createSipURI(uri: String): SipURI

  def createTelURL(phoneNumber: String): TelURL

  def createAddress(address: String): Address

  def createAddress(uri: URI): Address

  def createAddress(uri: URI, displayName: Option[String]): Address

}
