package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.FromHeader
import org.sisioh.sip.util.{Encoder, NameValuePairList, DuplicateNameValueList}
import org.sisioh.sip.message.address.impl.{AddressType, DefaultAddress}
import org.sisioh.sip.core.Separators

object From {

  def apply
  (address: DefaultAddress,
   parameters: NameValuePairList = NameValuePairList()) = new From(address, parameters)

  def fromTo(to: To) =
    new From(to.address)

  def unapply(from: From): Option[(DefaultAddress, NameValuePairList)] =
    Some(from.address, from.parameters)

  object JsonEncoder extends Encoder[To] {
    def encode(model: To, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("address", parse(model.address.encodeByJson())) ::
        JField("paramters", parse(model.parameters.encodeByJson())) :: Nil)
      builder.append(compact(render(json)))
    }
  }

}

class From
(val address: DefaultAddress,
 val parameters: NameValuePairList = NameValuePairList())
  extends AddressParametersHeader {

  val headerName = FromHeader.NAME
  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new From(address, _parameters)
  }

  def encodeBody(builder: StringBuilder) = {
    if (address.addressType == AddressType.ADDRESS) {
      builder.append(Separators.LESS_THAN)
    }
    address.encode(builder)
    if (address.addressType == AddressType.ADDRESS) {
      builder.append(Separators.GREATER_THAN)
    }
    if (!parameters.isEmpty) {
      builder.append(Separators.SEMICOLON)
      parameters.encode(builder)
    }
    builder
  }

  override def hashCode() = 31 * address.## + 31 * parameters.## + 31 * headerName.## + 31 * duplicates.##

  override def equals(obj: Any) = obj match {
    case that: To =>
      address == that.address &&
        parameters == that.parameters &&
        headerName == that.headerName &&
        duplicates == that.duplicates
    case _ => false
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, From.JsonEncoder)
}
