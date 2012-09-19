package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util.{NameValuePairList, DuplicateNameValueList}
import org.sisioh.sip.message.header.Parameters


trait ParametersHeader extends Parameters {

  val duplicates: DuplicateNameValueList

  val parameters: NameValuePairList


  def getParameter(name: String, stripQuotes: Boolean) = {
    parameters.getParameter(name, stripQuotes)
  }

  def getParameter(name: String) = parameters.getParameter(name)

  def getParameterValue(name: String) = parameters.getValue(name)
  def parameterNames = parameters.names
  def hasParameters = parameters.isEmpty == false
  def removeParameter(name: String) = {
    parameters.remove(name)
    this
  }

}
