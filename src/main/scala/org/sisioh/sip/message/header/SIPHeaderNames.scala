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

object SIPHeaderNames extends SIPHeaderNames {
  val values = List(
    MIN_EXPIRES,
    ERROR_INFO,
    MIME_VERSION,
    IN_REPLY_TO,
    ALLOW,
    CONTENT_LANGUAGE,
    VIA,
    ROUTE,
    PROXY_AUTHENTICATE,
    AUTHORIZATION,
    PROXY_AUTHORIZATION,
    WWW_AUTHENTICATE
  )
}

trait SIPHeaderNames {

  val MIN_EXPIRES = MinExpiresHeader.NAME

  val ERROR_INFO = ErrorInfoHeader.NAME

  val MIME_VERSION = MimeVersionHeader.NAME

  val IN_REPLY_TO = InReplyToHeader.NAME

  val ALLOW = AllowHeader.NAME

  val CONTENT_LANGUAGE = ContentLanguageHeader.NAME

  val CSEQ = CSeqHeader.NAME

  val CALL_ID = CallIdHeader.NAME


  val VIA = ViaHeader.NAME

  val ROUTE = RouteHeader.NAME

  val PROXY_AUTHORIZATION = ProxyAuthorizationHeader.NAME

  val AUTHORIZATION = AuthorizationHeader.NAME

  val PROXY_AUTHENTICATE = ProxyAuthenticateHeader.NAME

  val WWW_AUTHENTICATE = WWWAuthenticateHeader.NAME

}
