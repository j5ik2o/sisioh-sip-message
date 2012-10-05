package org.sisioh.sip.message.header

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

object ParameterNames extends ParameterNames

trait ParameterNames {
  val NEXT_NONCE = "nextnonce"

  val TAG = "tag"

  val USERNAME = "username"

  val URI = "uri"

  val DOMAIN = "domain"

  val CNONCE = "cnonce"

  val PASSWORD = "password"

  val RESPONSE = "response"

  val RESPONSE_AUTH = "rspauth"

  val OPAQUE = "opaque"

  val ALGORITHM = "algorithm"

  val DIGEST = "Digest"

  val SIGNED_BY = "signed-by"

  val SIGNATURE = "signature"

  val NONCE = "nonce"

  val SRAND = "srand"

  val SNUM = "snum"

  val TARGET_NAME = "targetname"

  // Issue reported by larryb
  val NONCE_COUNT = "nc"

  val PUBKEY = "pubkey"

  val COOKIE = "cookie"

  val REALM = "realm"

  val VERSION = "version"

  val STALE = "stale"

  val QOP = "qop"

  val NC = "nc"

  val PURPOSE = "purpose"

  val CARD = "card"

  val INFO = "info"

  val ACTION = "action"

  val PROXY = "proxy"

  val REDIRECT = "redirect"

  val PRAM_EXPIRES = "expires"

  val Q = "q"

  val RENDER = "render"

  val SESSION = "session"

  val ICON = "icon"

  val ALERT = "alert"

  val HANDLING = "handling"

  val REQUIRED = "required"

  val OPTIONAL = "optional"

  val EMERGENCY = "emergency"

  val URGENT = "urgent"

  val NORMAL = "normal"

  val NON_URGENT = "non-urgent"

  val DURATION = "duration"

  val BRANCH = "branch"

  val HIDDEN = "hidden"

  val RECEIVED = "received"

  val MADDR = "maddr"

  val TTL = "ttl"

  val TRANSPORT = "transport"

  val TEXT = "text"

  val CAUSE = "cause"

  val ID = "id"

  // @@@ hagai
  val RPORT = "rport"

  // Added pmusgrave (Replaces support)
  val TO_TAG = "to-tag"
  val FROM_TAG = "from-tag"

  // pmusgrave (outbound and gruu)
  // draft-sip-outbouund-08
  // draft-sip-gruu-12
  val SIP_INSTANCE = "+sip.instance"
  val PUB_GRUU = "pub-gruu"
  val TEMP_GRUU = "temp-gruu"
  val GRUU = "gruu"
}
