package org.sisioh.sip.message.address

import org.sisioh.sip.util.Encodable

@cloneable
trait Address extends Serializable {

  val displayName: Option[String]
  val uri: URI
  val isWildcard: Boolean

}




