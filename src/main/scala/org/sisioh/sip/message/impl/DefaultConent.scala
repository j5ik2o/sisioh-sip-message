package org.sisioh.sip.message.impl

/**
 * Created with IntelliJ IDEA.
 * User: junichi_kato
 * Date: 12/09/24
 * Time: 19:15
 * To change this template use File | Settings | File Templates.
 */

import org.sisioh.sip.message.header.{Header, ContentDispositionHeader, ContentTypeHeader}
import org.sisioh.sip.core.Separators

case class DefaultConent
(content: Any,
 contentTypeHeader: Option[ContentTypeHeader],
 contentDispositionHeader: Option[ContentDispositionHeader]
  ) extends Conent {

  private val _extensionHeaders: List[Header] = List()

  def extensionHeaders: Iterator[Header] = _extensionHeaders.iterator

  override def toString = {
    val sb = new StringBuilder
    if (contentTypeHeader.isDefined) {
      sb.append(contentTypeHeader.get.toString)
    }
    if (contentDispositionHeader.isDefined){
      sb.append(contentDispositionHeader.get.toString)
    }
    _extensionHeaders.foreach{
      e =>
        sb.append(e)
    }
    sb.append(Separators.NEWLINE)
    sb.append(content.toString)
    sb.result()
  }
}
