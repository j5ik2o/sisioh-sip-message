package org.sisioh.sip.message.address

@cloneable
trait Address extends Serializable {

  val displayName: Option[String]
  val uri: URI
  val isWildcard: Boolean

}




