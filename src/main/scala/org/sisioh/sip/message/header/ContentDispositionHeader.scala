package org.sisioh.sip.message.header

/**
 * Created with IntelliJ IDEA.
 * User: junichi_kato
 * Date: 12/09/12
 * Time: 15:19
 * To change this template use File | Settings | File Templates.
 */
trait ContentDispositionHeader extends Parameters with Header {
  val dispositionType: Int
  val handling: String
}

object ContentDispositionHeader {
  val NAME = "Content-Disposition"
  val SESSION = "Session"
  val RENDER = "Render"
  val ICON = "Icon"
  val ALERT = "Alert"
}
