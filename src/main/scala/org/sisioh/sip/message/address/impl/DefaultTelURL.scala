package org.sisioh.sip.message.address.impl

import org.sisioh.sip.message.address.TelURL
import org.sisioh.sip.core.GenericObject
import org.sisioh.sip.util.{Encoder, Decoder, ParserBase}

object DefaultTelURLDecoder {
  def apply() = new DefaultTelURLDecoder
}

class DefaultTelURLDecoder extends Decoder with DefaultTelURLParser {
  def decode(source: String): DefaultTelURL = decodeTarget(source, defaultTelURL)
}

trait DefaultTelURLParser extends ParserBase with TelephoneNumberParser {

  def defaultTelURL : Parser[DefaultTelURL] = "tel:" ~> telephoneNumber ^^ {
    t => DefaultTelURL(t)
  }
}

object DefaultTelURL {

  def apply(telephoneNumber: TelephoneNumber, phoneContext: Option[String] = None) =
    new DefaultTelURL(telephoneNumber, phoneContext)

  def unapply(defaultTelURL:DefaultTelURL): Option[(TelephoneNumber, Option[String])] =
    Some(defaultTelURL.telephoneNumber, defaultTelURL.phoneContext)

  def decode(telephoneNumber: String) = DefaultTelURLDecoder().decode(telephoneNumber)

  object JsonEncoder extends Encoder[DefaultTelURL]{
    def encode(model: DefaultTelURL, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("scheme", JString(model.scheme)) ::
        JField("isGlobal", JBool(model.isGlobal)) ::
        JField("phoneNumber", JString(model.phoneNumber)) ::
        JField("parameters", parse(model.params.encodeByJson())) :: Nil)
      builder.append(compact(render(json)))
    }
  }
}

class DefaultTelURL
(telephoneNumberParam: TelephoneNumber,
 phoneContextParam: Option[String] = None)
  extends TelURL with GenericObject {

  val telephoneNumber: TelephoneNumber = phoneContextParam.map {
    pc => telephoneNumberParam.withParameter("phone-context", pc)
  }.getOrElse(telephoneNumberParam)

  val phoneContext = if (telephoneNumber.hasParams("phone-context"))
    telephoneNumber.getParameter("phone-context")
  else phoneContextParam

  def parameterNames = telephoneNumber.parameterNames

  val parameters = telephoneNumber.params

  def withPostDial(postDial: String) = withParameter(ParameterNames.POSTDIAL, postDial)

  def withIsdnSubaddress(subAddress: String) = withParameter(ParameterNames.ISUB, subAddress)

  def withParameter(name: String, value: Any) =
    new DefaultTelURL(
      telephoneNumber.withParameter(name, value),
      None
    )

  def getParameter(name: String) = telephoneNumber.getParameter(name)

  def removePhoneContext = {
    new DefaultTelURL(
      telephoneNumber.removeParameter("phone-context"),
      None
    )
  }

  def removeParameter(name: String) = {
    new DefaultTelURL(
      telephoneNumber.removeParameter(name),
      phoneContext
    )
  }

  val isGlobal = telephoneNumber.isGlobal
  val phoneNumber = telephoneNumber.phoneNumber
  val postDial = telephoneNumber.postDial
  val isdnSubAddress = telephoneNumber.isdnSubaddress
  val hasPostDial = telephoneNumber.hasPostDial
  val hasIsdnSubAddress = telephoneNumber.hasIsdnSubAddress
  val params = telephoneNumber.params

  val scheme = "tel"
  val isSipURI = false

  def encode(builder: StringBuilder) = {
    builder.append(scheme).append(':')
    telephoneNumber.encode(builder)
    builder
  }

  override def hashCode() = 31 * telephoneNumber.##

  override def equals(obj: Any) = obj match {
    case that: DefaultTelURL =>
      telephoneNumber == that.telephoneNumber
    case _ => false
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, DefaultTelURL.JsonEncoder)
}
