package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ContentTypeHeader

/**
 * Created with IntelliJ IDEA.
 * User: junichi_kato
 * Date: 12/09/18
 * Time: 20:40
 * To change this template use File | Settings | File Templates.
 */
case class ContentType(contentType: String, contentSubType: Option[String]) extends ContentTypeHeader {
  val name = ContentTypeHeader.NAME

  def parameterNames = null

  def getParameter(name: String) = null

  def removeParameter(name: String) = null
}
