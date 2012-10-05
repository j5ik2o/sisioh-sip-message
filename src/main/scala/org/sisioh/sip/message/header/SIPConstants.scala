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
