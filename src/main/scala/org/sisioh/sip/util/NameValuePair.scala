package org.sisioh.sip.util

import scala.Tuple2
import org.sisioh.sip.core.{GenericObjectList, GenericObject, Separators}

object NameValuePair {
  def apply[A]
  (name: Option[String],
   valueParam: Option[A],
   isFlagValueType: Boolean = false) = new NameValuePair(name, valueParam, isFlagValueType)

  def unapply[A](nameValuePair: NameValuePair): Option[(Option[String], Option[Any], Boolean)] =
    Some(nameValuePair.name, nameValuePair.value, nameValuePair.isFlagValueType)

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
 valueParam: Option[Any],
 val isFlagValueType: Boolean = false,
 val separator: String = Separators.EQUALS)
  extends Tuple2[Option[String], Option[Any]](name, valueParam) with GenericObject[NameValuePair] {

  val value: Option[Any] = if (isFlagValueType) None else valueParam

  override def hashCode() =
    31 * name.## + 31 * value.##

  override def equals(obj: Any) = obj match {
    case that: NameValuePair =>
      name == that.name && value == that.value
    case _ => false
  }

  override def toString() = encode()

  def encode(builder: StringBuilder) = {
    (name, value, isFlagValueType) match {
      case (Some(n), Some(v), false) =>
        val encodeValue = if (v.isInstanceOf[GenericObject[_]]) {
          v.asInstanceOf[GenericObject[_]].encode()
        } else if (v.isInstanceOf[GenericObjectList[_]]) {
          v.asInstanceOf[GenericObjectList[_]].encode()
        } else {
          v.toString
        }
        builder.append("%s%s%s".format(n, separator, encodeValue))
      case (None, Some(v), _) =>
        val encodeValue = if (v.isInstanceOf[GenericObject[_]]) {
          v.asInstanceOf[GenericObject[_]].encode()
        } else if (v.isInstanceOf[GenericObjectList[_]]) {
          v.asInstanceOf[GenericObjectList[_]].encode()
        } else {
          v.toString
        }
        builder.append(encodeValue)
    }

  }
}
