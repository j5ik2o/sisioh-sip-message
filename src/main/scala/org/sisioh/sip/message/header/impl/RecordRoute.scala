package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.RecordRouteHeader
import org.sisioh.sip.message.address.impl.{AddressType, DefaultAddress}
import org.sisioh.sip.util.{NameValuePairList, DuplicateNameValueList}
import org.sisioh.sip.core.Separators

case class RecordRoute
(address: DefaultAddress,
 parameters: NameValuePairList = NameValuePairList())
  extends AddressParametersHeader with RecordRouteHeader {

  type ParametersHeaderType = RecordRoute

  val name = RecordRouteHeader.NAME
  val headerName = name

  def encodeAsJValue() = {
    // TODO
    null
  }

  def encodeBody(builder: StringBuilder) = {
    if (address.addressType == AddressType.ADDRESS) {
      builder.append(Separators.LESS_THAN)
    }
    address.encode(builder)
    if (address.addressType == AddressType.ADDRESS) {
      builder.append(Separators.GREATER_THAN)
    }

    if (!parameters.isEmpty) {
      builder.append(Separators.SEMICOLON)
      this.parameters.encode(builder)
    }
    builder
  }

  protected def createParametersHeader(duplicates: DuplicateNameValueList, _parameters: NameValuePairList) =
    new RecordRoute(address, _parameters)

  val duplicates = DuplicateNameValueList()
}
