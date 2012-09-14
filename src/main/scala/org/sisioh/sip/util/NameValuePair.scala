package org.sisioh.sip.util

import scala.Tuple2
import org.sisioh.sip.core.Separators

object NameValuePair {
  def apply[A]
  (name: String,
   valueParam: Option[A],
   isFlagValueType: Boolean = false,
   separatorParam: Option[String] = None) = new NameValuePair[A](name, valueParam, isFlagValueType, separatorParam)

  def unapply[A](nameValuePair: NameValuePair[A]): Option[(String, Option[A], Boolean, String)] =
    Some(nameValuePair.name, nameValuePair.value, nameValuePair.isFlagValueType, nameValuePair.separator)

}


class NameValuePair[A]
(val name: String,
 valueParam: Option[A],
 val isFlagValueType: Boolean = false,
 separatorParam: Option[String] = None)
  extends Tuple2[String, Option[A]](name, valueParam) {

  val separator = separatorParam.getOrElse(Separators.EQUALS)
  val value: Option[A] = if (isFlagValueType) None else valueParam
  val isQuotedString = false
  val quotes = ""

  def buildQuoted: NameValuePair[A] = new NameValuePair[A](name, valueParam, isFlagValueType, separatorParam) {
    override val isQuotedString = true
    override val quotes = Separators.DOUBLE_QUOTE
  }

  override def hashCode() = {
    31 * name.## + 31 * value.##
  }

  override def equals(obj: Any) = obj match {
    case that: NameValuePair[_] =>
      name == that.name && value == that.value
    case _ => false
  }
}
