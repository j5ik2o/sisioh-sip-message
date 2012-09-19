package org.sisioh.sip.message.address

trait URI extends Serializable {

  val scheme: String
  val isSipURI: Boolean

}
