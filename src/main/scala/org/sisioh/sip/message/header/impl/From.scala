package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.FromHeader
import org.sisioh.sip.util.{NameValuePairList, DuplicateNameValueList}
import org.sisioh.sip.message.address.impl.{AddressType, DefaultAddress}
import org.sisioh.sip.core.Separators

object From {

  def apply
  (address: DefaultAddress,
   parameters: NameValuePairList) = new From(address, parameters)

  def fromTo(to: To) = {
    new From(to.address)
  }

}

class From
(val address: DefaultAddress,
 val parameters: NameValuePairList = NameValuePairList())
  extends AddressParametersHeader {

  val headerName = FromHeader.NAME
  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new From(address, _parameters)
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
