package org.sisioh.sip.core

import org.sisioh.sip.util.Encodable

/**
 * Created with IntelliJ IDEA.
 * User: junichi
 * Date: 2012/09/16
 * Time: 18:05
 * To change this template use File | Settings | File Templates.
 */
trait GenericObjectList[A] extends Encodable[A] {
  this: A =>
}
