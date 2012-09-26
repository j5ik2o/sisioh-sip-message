package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util.{NameValuePairList, DuplicateNameValueList}
import org.sisioh.sip.message.header.Parameters


trait ParametersHeader extends Parameters with ParametersHeaderExt with SIPHeader {

  type ParametersHeaderType <: ParametersHeader

  val duplicates: DuplicateNameValueList

  val parameters: NameValuePairList

  protected def createParametersHeader(duplicates: DuplicateNameValueList, parameters: NameValuePairList): ParametersHeaderType

  def getParameter(name: String, stripQuotes: Boolean) =
    parameters.getParameter(name, stripQuotes)

  def getParameter(name: String) = parameters.getParameter(name)

  def getParameterValue(name: String) = parameters.getValue(name)

  def parameterNames = parameters.names

  def hasParameters = parameters.isEmpty == false

  def withParameter(name: String, value: Any): ParametersHeaderType =
    createParametersHeader(duplicates, parameters.add(name, value))

  def removeParameter(name: String): ParametersHeaderType =
    createParametersHeader(duplicates, parameters.remove(name))

}
