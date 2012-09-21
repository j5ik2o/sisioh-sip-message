package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ContentTypeHeader

case class ContentType(contentType: String, contentSubType: Option[String]) extends ContentTypeHeader {
  val name = ContentTypeHeader.NAME

  def parameterNames = null

  def getParameter(name: String) = null

  def removeParameter(name: String) = null
}
