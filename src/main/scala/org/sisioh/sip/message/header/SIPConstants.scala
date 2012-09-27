package org.sisioh.sip.message.header

object SIPConstants extends SIPHeaderNames with org.sisioh.sip.message.address.ParameterNames with org.sisioh.sip.message.header.ParameterNames {
  val DEFAULT_PORT = 5060

  // Added by Daniel J. Martinez Manzano <dani@dif.um.es>
  val DEFAULT_TLS_PORT = 5061

  /**
   * Prefix for the branch parameter that identifies
   * BIS 09 compatible branch strings. This indicates
   * that the branch may be as a global identifier for
   * identifying transactions.
   */
  val BRANCH_MAGIC_COOKIE = "z9hG4bK"

  val BRANCH_MAGIC_COOKIE_LOWER_CASE = "z9hg4bk"

  val BRANCH_MAGIC_COOKIE_UPPER_CASE = "Z9HG4BK"

  /**
   * constant SIP_VERSION_STRING
   */
  val SIP_VERSION_STRING = "SIP/2.0"
}
