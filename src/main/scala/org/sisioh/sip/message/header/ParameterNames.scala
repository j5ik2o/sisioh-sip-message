package org.sisioh.sip.message.header

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

  val EXPIRES = "expires"

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