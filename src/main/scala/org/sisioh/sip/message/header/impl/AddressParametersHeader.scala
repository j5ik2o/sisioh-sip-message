package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.address.impl.DefaultAddress

trait AddressParametersHeader extends ParametersHeader {

  val address: DefaultAddress



}
