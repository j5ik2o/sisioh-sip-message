package org.sisioh.sip.core

import org.sisioh.sip.util.Encodable

trait GenericObject[A] extends Encodable[A] {
  this : A =>

}
