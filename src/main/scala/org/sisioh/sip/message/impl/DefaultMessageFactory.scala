package org.sisioh.sip.message.impl

import org.sisioh.sip.message.header._
import impl._
import org.sisioh.sip.message.{Response, StatusCode, Request, MessageFactory}
import org.sisioh.sip.message.address.URI
import org.sisioh.sip.message.address.impl.GenericURI

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

object DefaultMessageFactory {

  val defaultContentEncodingCharset = "UTF-8"

  def apply(userAgent: Option[UserAgentHeader], server: Option[ServerHeader]) = new DefaultMessageFactory(userAgent, server)

}

class DefaultMessageFactory(userAgent: Option[UserAgentHeader], server: Option[ServerHeader]) extends MessageFactory {

  def createRequest
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Any) = {
    SIPRequestBuilder().
      withUri(requestURI.asInstanceOf[GenericURI]).
      withMethod(Some(method)).
      withCallId(Some(callId.asInstanceOf[CallId])).
      withCSeq(Some(cseq.asInstanceOf[CSeq])).
      withFrom(Some(from.asInstanceOf[From])).
      withTo(Some(to.asInstanceOf[To])).
      withMessageContent(Some(MessageContent(content.toString, Some(contentType.asInstanceOf[ContentType])))).
      withContentLength(Some(ContentLength(content.toString.getBytes.length))).
      withHeaders(userAgent.map(_.asInstanceOf[UserAgent]).toList).
      build
  }

  def createRequest
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Array[Byte]) = {
    SIPRequestBuilder().
      withUri(requestURI.asInstanceOf[GenericURI]).
      withMethod(Some(method)).
      withCallId(Some(callId.asInstanceOf[CallId])).
      withCSeq(Some(cseq.asInstanceOf[CSeq])).
      withFrom(Some(from.asInstanceOf[From])).
      withTo(Some(to.asInstanceOf[To])).
      withMessageContent(Some(MessageContent(content, Some(contentType.asInstanceOf[ContentType])))).
      withContentLength(Some(ContentLength(content.length))).
      withHeaders(userAgent.map(_.asInstanceOf[UserAgent]).toList).
      build
  }

  def createRequest
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader) = {
    SIPRequestBuilder().
      withUri(requestURI.asInstanceOf[GenericURI]).
      withMethod(Some(method)).
      withCallId(Some(callId.asInstanceOf[CallId])).
      withCSeq(Some(cseq.asInstanceOf[CSeq])).
      withFrom(Some(from.asInstanceOf[From])).
      withTo(Some(to.asInstanceOf[To])).
      withContentLength(Some(ContentLength(0))).
      withHeaders(userAgent.map(_.asInstanceOf[UserAgent]).toList).
      build
  }

  def createResponse
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Any): Response = {
    SIPResponseBuilder().
      withStatusCode(statusCode).
      withCallId(Some(callId.asInstanceOf[CallId])).
      withCSeq(Some(cseq.asInstanceOf[CSeq])).
      withFrom(Some(from.asInstanceOf[From])).
      withTo(Some(to.asInstanceOf[To])).
      withContentLength(Some(ContentLength(0))).
      withHeaders(server.map(_.asInstanceOf[Server]).toList).
      build
  }

  def createResponse
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Array[Byte]) = {
    SIPResponseBuilder().
      withStatusCode(statusCode).
      withCallId(Some(callId.asInstanceOf[CallId])).
      withCSeq(Some(cseq.asInstanceOf[CSeq])).
      withFrom(Some(from.asInstanceOf[From])).
      withTo(Some(to.asInstanceOf[To])).
      withMessageContent(Some(MessageContent(content, Some(contentType.asInstanceOf[ContentType])))).
      withContentLength(Some(ContentLength(content.length))).
      withHeaders(server.map(_.asInstanceOf[Server]).toList).
      build
  }

  def createResponse
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader) = {
    SIPResponseBuilder().
      withStatusCode(statusCode).
      withCallId(Some(callId.asInstanceOf[CallId])).
      withCSeq(Some(cseq.asInstanceOf[CSeq])).
      withFrom(Some(from.asInstanceOf[From])).
      withTo(Some(to.asInstanceOf[To])).
      withContentLength(Some(ContentLength(0))).
      withHeaders(server.map(_.asInstanceOf[Server]).toList).
      build
  }

  def createResponse
  (statusCode: StatusCode.Value,
   request: Request,
   contentType: ContentTypeHeader,
   content: Any) = {
    val messageContent = Some(MessageContent(content.toString.getBytes, Some(contentType.asInstanceOf[ContentType])))
    request.asInstanceOf[SIPRequest].createResponse(statusCode, None, messageContent)
  }

  def createResponse
  (statusCode: StatusCode.Value,
   request: Request,
   contentType: ContentTypeHeader,
   content: Array[Byte]) = {
    val messageContent = Some(MessageContent(content, Some(contentType.asInstanceOf[ContentType])))
    request.asInstanceOf[SIPRequest].createResponse(statusCode, None, messageContent)
  }

  def createResponse(statusCode: StatusCode.Value, request: Request) = {
    request.asInstanceOf[SIPRequest].createResponse(statusCode)
  }

  def createRequest(requestString: String) = {
    null
  }

  def createResponse(responseString: String) = null
}
