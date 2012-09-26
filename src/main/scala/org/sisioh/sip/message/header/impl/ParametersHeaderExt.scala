package org.sisioh.sip.message.header.impl

trait ParametersHeaderExt {

  def getParameter(name: String, stripQuotes: Boolean): Option[String]

}
