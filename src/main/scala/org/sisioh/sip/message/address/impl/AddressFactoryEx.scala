package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.SipURI

trait AddressFactoryEx {
  def createSipURI(sipUri: String): SipURI
}
