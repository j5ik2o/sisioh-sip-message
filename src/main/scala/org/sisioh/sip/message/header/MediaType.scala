package org.sisioh.sip.message.header

/**
 * Created with IntelliJ IDEA.
 * User: junichi_kato
 * Date: 12/09/12
 * Time: 15:27
 * To change this template use File | Settings | File Templates.
 */
trait MediaType {
  val contentType: String

  val contentSubType: String
}
