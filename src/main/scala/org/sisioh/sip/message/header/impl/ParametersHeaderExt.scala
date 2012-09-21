package org.sisioh.sip.message.header.impl

trait ParametersHeaderExt {
  this: ParametersHeader =>

  def getParameter(name: String, stripQuotes: Boolean): Option[String]

}
