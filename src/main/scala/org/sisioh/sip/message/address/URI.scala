package org.sisioh.sip.message.address

@cloneable
trait URI extends Serializable {

  val scheme: String
  val isSipURI: Boolean

}
