package org.sisioh.sip.util

import scala.Tuple2
import org.sisioh.sip.core.{GenericObjectList, GenericObject, Separators}

object NameValuePair {
  def apply[A]
  (name: Option[String],
   value: Option[A],
   separator: String = Separators.EQUALS,
   quotes: String = "",
   isQuotedString: Boolean = false) = new NameValuePair(name, value, separator, quotes, isQuotedString)

  def unapply[A](nameValuePair: NameValuePair): Option[(Option[String], Option[Any])] =
    Some(nameValuePair.name, nameValuePair.value)

  class JsonEncoder[A] extends Encoder[NameValuePair] {
    def encode(model: NameValuePair, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(model.value.map {
        e =>
          JField("name", JString(model.name.get)) :: JField("value", e match {
            case s: String => JString(s)
            case v: GenericObject[_] =>
              parse(v.encode())
            case v: GenericObjectList[_] =>
              parse(v.encode())
          }) :: Nil
      }.getOrElse {
        JField("name", JString(model.name.get)) :: Nil
      })
      builder.append(compact(render(json)))
    }
  }

}


class NameValuePair
(val name: Option[String],
 val value: Option[Any],
 val separator: String = Separators.EQUALS,
 val quotes: String = "",
 val isQuotedString: Boolean = false)
  extends Tuple2[Option[String], Option[Any]](name, value) with GenericObject[NameValuePair] {

  override def hashCode() =
    31 * name.## + 31 * value.##

  override def equals(obj: Any) = obj match {
    case that: NameValuePair =>
      name == that.name && value == that.value
    case _ => false
  }

  override def toString() = encode()

  def encode(builder: StringBuilder) = {
    (name, value) match {
      case (Some(n), Some(v)) if (v.isInstanceOf[Boolean] == false) =>
        if (v.isInstanceOf[GenericObject[_]]) {
          builder.append(n).append(separator).append(quotes)
          v.asInstanceOf[GenericObject[_]].encode(builder)
          builder.append(quotes)
        } else if (v.isInstanceOf[GenericObjectList[_]]) {
          builder.append(n).append(separator).append(v.asInstanceOf[GenericObjectList[_]].encode())
        } else if (v.toString.size == 0) {
          if (isQuotedString) {
            builder.append(n).append(separator).append(quotes).append(quotes)
          } else {
            builder.append(n).append(separator)
          }
        } else {
          builder.append(n).append(separator).append(quotes).append(v.toString).append(quotes)
        }
      case (None, Some(v)) =>
        if (v.isInstanceOf[GenericObject[_]]) {
          builder.append(v.asInstanceOf[GenericObject[_]].encode())
        } else if (v.isInstanceOf[GenericObjectList[_]]) {
          builder.append(v.asInstanceOf[GenericObjectList[_]].encode())
        } else {
          builder.append(quotes).append(v.toString).append(quotes)
        }
      case (Some(n), Some(v)) if (v.isInstanceOf[Boolean] && v.asInstanceOf[Boolean]) =>
        builder.append(n)
      case _ =>
        builder
    }

  }
}
