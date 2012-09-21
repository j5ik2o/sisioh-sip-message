package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ToHeader
import org.sisioh.sip.util.{NameValuePairList, DuplicateNameValueList}
import org.sisioh.sip.message.address.impl.{AddressType, DefaultAddress}
import org.sisioh.sip.core.Separators

object To {
  def apply
  (address: DefaultAddress,
   parameters: NameValuePairList = NameValuePairList()) = new To(address, parameters)

  def fromFrom(from: From) =
    new To(from.address)
}

class To
(val address: DefaultAddress,
 val parameters: NameValuePairList = NameValuePairList())
  extends AddressParametersHeader {

  val headerName = ToHeader.NAME
  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new To(address, _parameters)
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
      parameters.encode(builder)
    }
    builder
  }

}
