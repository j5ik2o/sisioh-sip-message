package org.sisioh.sip.message.header

/*
 * Copyright 2012 Sisioh Project and others. (http://www.sisioh.org/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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
    CALL_INFO,
    CSEQ,
    ALERT_INFO,
    ACCEPT_ENCODING,
    ACCEPT,
    ACCEPT_LANGUAGE,
    RECORD_ROUTE,
    TIMESTAMP,
    TO,
    VIA,
    FROM,
    CALL_ID,
    AUTHORIZATION,
    PROXY_AUTHENTICATE,
    SERVER,
    UNSUPPORTED,
    RETRY_AFTER,
    CONTENT_TYPE,
    CONTENT_ENCODING,
    CONTENT_LENGTH,
    ROUTE,
    CONTACT,
    WWW_AUTHENTICATE,
    MAX_FORWARDS,
    PROXY_AUTHORIZATION,
    CONTENT_DISPOSITION,
    SUBJECT,
    USER_AGENT,
    WARNING,
    PRIORITY,
    DATE,
    EXPIRES,
    SUPPORTED,
    AUTHENTICATION_INFO,
    REPLY_TO,
    RACK,
    RSEQ,
    REASON,
    SUBSCRIPTION_STATE,
    EVENT,
    ALLOW_EVENTS,
    SIP_ETAG,
    SIP_IF_MATCH,
    REFERRED_BY,
    SESSION_EXPIRES,
    MIN_SE,
    REPLACES,
    JOIN
  )
}

trait SIPHeaderNames {

  val MIN_EXPIRES = MinExpiresHeader.NAME
  //1
  val ERROR_INFO = ErrorInfoHeader.NAME
  //2
  val MIME_VERSION = MimeVersionHeader.NAME
  //3
  val IN_REPLY_TO = InReplyToHeader.NAME
  //4
  val ALLOW = AllowHeader.NAME
  //5
  val CONTENT_LANGUAGE = ContentLanguageHeader.NAME
  //6
  val CALL_INFO = CallInfoHeader.NAME
  //7
  val CSEQ = CSeqHeader.NAME
  //8
  val ALERT_INFO = AlertInfoHeader.NAME
  //9
  val ACCEPT_ENCODING = AcceptEncodingHeader.NAME
  //10
  val ACCEPT = AcceptHeader.NAME
  //11
  val ACCEPT_LANGUAGE = AcceptLanguageHeader.NAME
  //12
  val RECORD_ROUTE = RecordRouteHeader.NAME
  //13
  val TIMESTAMP = TimeStampHeader.NAME
  //14
  val TO = ToHeader.NAME
  //15
  val VIA = ViaHeader.NAME
  //16
  val FROM = FromHeader.NAME
  //17
  val CALL_ID = CallIdHeader.NAME
  //18
  val AUTHORIZATION = AuthorizationHeader.NAME
  //19
  val PROXY_AUTHENTICATE =
    ProxyAuthenticateHeader.NAME
  //20
  val SERVER = ServerHeader.NAME
  //21
  val UNSUPPORTED = ""
  //UnsupportedHeader.NAME; //22
  val RETRY_AFTER = ""
  //RetryAfterHeader.NAME; //23
  val CONTENT_TYPE = ContentTypeHeader.NAME
  //24
  val CONTENT_ENCODING = ContentEncodingHeader.NAME
  //25
  val CONTENT_LENGTH = ContentLengthHeader.NAME
  //26
  val ROUTE = RouteHeader.NAME
  //27
  val CONTACT = ContactHeader.NAME
  //28
  val WWW_AUTHENTICATE = WWWAuthenticateHeader.NAME
  //29
  val MAX_FORWARDS = MaxForwardsHeader.NAME
  //30
  //val ORGANIZATION = OrganizationHeader.NAME; //31
  val PROXY_AUTHORIZATION =
    ProxyAuthorizationHeader.NAME
  //32
  //val PROXY_REQUIRE = ProxyRequireHeader.NAME; //33
  //val REQUIRE = RequireHeader.NAME; //34
  val CONTENT_DISPOSITION =
    ContentDispositionHeader.NAME
  //35
  val SUBJECT = ""
  //SubjectHeader.NAME; //36
  val USER_AGENT = UserAgentHeader.NAME
  //37
  val WARNING = ""
  //WarningHeader.NAME; //38
  val PRIORITY = ""
  //PriorityHeader.NAME; //39
  val DATE = ""
  //DateHeader.NAME; //40
  val EXPIRES = ExpiresHeader.NAME
  //41
  val SUPPORTED = ""
  //SupportedHeader.NAME; //42
  val AUTHENTICATION_INFO = ""
  //AuthenticationInfoHeader.NAME
  //43
  val REPLY_TO = ""
  //ReplyToHeader.NAME; //44
  val RACK = ""
  //RAckHeader.NAME; //45
  val RSEQ = ""
  //RSeqHeader.NAME; //46
  val REASON = ""
  //ReasonHeader.NAME; //47
  val SUBSCRIPTION_STATE =
    SubscriptionStateHeader.NAME
  //48
  val EVENT = EventHeader.NAME
  //44
  val ALLOW_EVENTS = "" //AllowEventsHeader.NAME; //45

  val SIP_ETAG = ""
  //SIPETagHeader.NAME; //46
  val SIP_IF_MATCH = "" //SIPIfMatchHeader.NAME; //47

  // NewHeights pmusgrave
  val REFERRED_BY = ""
  //ReferredByHeader.NAME; //48
  val SESSION_EXPIRES = ""
  //SessionExpiresHeader.NAME; //49
  val MIN_SE = ""
  //MinSEHeader.NAME; //50
  val REPLACES = ""
  //ReplacesHeader.NAME; //51
  val JOIN = "" //JoinHeader.NAME; //52
}
