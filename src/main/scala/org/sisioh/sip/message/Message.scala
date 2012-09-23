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

trait Message[T] {

  def addHeader(header: Header): Message[T]

  def addLast(header: Header): Message[T]

  def addFirst(header: Header): Message[T]

  def removeFirst(headerName: String): Message[T]

  def removeLast(headerName: String): Message[T]

  def removeHeader(headerName: String): Message[T]

  def getHeaderNames: Iterator[String]

  def getHeaders(headerName: String): Iterator[Header]

  def getHeader(headerName: String): String

  def getUnrecognizedHeaders: Iterator[Header]

  def getContentLength: ContentLengthHeader

  def getContentLanguage: ContentLanguageHeader

  def getContentEncoding: ContentEncodingHeader

  def getContentType: ContentTypeHeader

  def getRawContent: Array[Byte]

  def getContent: Option[T]

  def remoteContent: Message[T]

  def getExpires: ExpiresHeader

  def getSIPVersion: String
}
