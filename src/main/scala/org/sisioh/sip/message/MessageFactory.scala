package org.sisioh.sip.message

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

import address.URI
import header._

trait MessageFactory {

  def createRequest
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Any): Request

  def createRequest
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Array[Byte]): Request

  def createRequest
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader): Request


  def createResponse
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Any): Response

  def createResponse
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Array[Byte]): Response

  def createResponse
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader): Response

  def createResponse
  (statusCode: StatusCode.Value,
   request: Request,
   contentType: ContentTypeHeader,
   content: Any): Response

  def createResponse
  (statusCode: StatusCode.Value,
   request: Request,
   contentType: ContentTypeHeader,
   content: Array[Byte]): Response

  def createResponse
  (statusCode: StatusCode.Value,
   request: Request): Response

  def createRequest(requestString: String): Request

  def createResponse(responseString: String): Response


}
