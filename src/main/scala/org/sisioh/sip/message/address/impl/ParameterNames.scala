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


import org.sisioh.sip.message.address.NetObject

object ParameterNames {
  val SIP_URI_SCHEME = "sip"
  val SIPS_URI_SCHEME = "sips"
  val TEL_URI_SCHEME = "tel"
  val POSTDIAL = "postd"
  val PHONE_CONTEXT_TAG = "context-tag"
  val ISUB = "isub"
  val PROVIDER_TAG = "provider-tag";
  val UDP = NetObject.UDP
  val TCP = NetObject.TCP
  val TLS = NetObject.TLS
}
