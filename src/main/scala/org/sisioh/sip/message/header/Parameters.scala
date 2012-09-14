package org.sisioh.sip.message.header

trait Parameters {

  def parameterNames: Iterator[String]

  def getParameter(name: String): String

  def removeParameter(name :String): Parameters

}
