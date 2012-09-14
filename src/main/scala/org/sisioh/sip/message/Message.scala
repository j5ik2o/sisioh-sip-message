package org.sisioh.sip.message

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
