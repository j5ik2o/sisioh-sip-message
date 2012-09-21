package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util.{NameValuePairList, DuplicateNameValueList}
import org.sisioh.sip.message.header.Parameters


trait ParametersHeader extends Parameters with ParametersHeaderExt with SIPHeader {

  val duplicates: DuplicateNameValueList

  val parameters: NameValuePairList

  protected def createParametersHeader(duplicates: DuplicateNameValueList, parameters: NameValuePairList): ParametersHeader

  def getParameter(name: String, stripQuotes: Boolean) =
    parameters.getParameter(name, stripQuotes)

  def getParameter(name: String) = parameters.getParameter(name)

  def getParameterValue(name: String) = parameters.getValue(name)

  def parameterNames = parameters.names

  def hasParameters = parameters.isEmpty == false

  def removeParameter(name: String) =
    createParametersHeader(duplicates, parameters.remove(name))

}
