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

  def createRequest[A]
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: A): Request[A]

  def createRequest
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Array[Byte]): Request[Array[Byte]]

  def createRequest[T]
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader): Request[T]


  def createResponse[A]
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: A): Response[A]

  def createResponse
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Array[Byte]): Response[Array[Byte]]

  def createResponse[A]
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader): Response[A]

  def createResponse[A, B]
  (statusCode: StatusCode.Value,
   request: Request[A],
   contentType: ContentTypeHeader,
   content: B): Response[B]

  def createResponse[A]
  (statusCode: StatusCode.Value,
   request: Request[A],
   contentType: ContentTypeHeader,
   content: Array[Byte]): Response[Array[Byte]]

  def createResponse[A, B]
  (statusCode: StatusCode.Value,
   request: Request[A]): Response[B]

  def createRequest[A](requestString: String): Request[A]

  def createResponse[A](responseString: String): Response[A]


}
