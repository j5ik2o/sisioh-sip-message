package org.sisioh.sip.message.header

import org.sisioh.sip.util.Encoder

trait Parameters {

  def parameterNames: Iterator[String]

  def getParameter(name: String): Option[Any]

  def removeParameter(name :String): Parameters

}
