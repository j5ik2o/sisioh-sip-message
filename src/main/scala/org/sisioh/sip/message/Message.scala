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

import header._

trait Message {

  def addHeader(header: Header): Message

  def addLast(header: Header): Message

  def addFirst(header: Header): Message

  def removeFirst(headerName: String): Message

  def removeLast(headerName: String): Message

  def removeHeader(headerName: String): Message

  def getHeaderNames: Iterator[String]

  def getHeaders(headerName: String): Iterator[Header]

  def getHeader(headerName: String): Option[Header]

  def unrecognizedHeaders: List[Header]

  def contentLength: Option[ContentLengthHeader]

  def contentLanguage: Option[ContentLanguageHeader]

  def contentEncoding: Option[ContentEncodingHeader]

  def contentType: Option[ContentTypeHeader]

  def expires: Option[ExpiresHeader]

  def getRawContent: Option[Array[Byte]]

  def getContent: Option[Any]

  def removeContent: Message

  val sipVersion: Option[String]
}
