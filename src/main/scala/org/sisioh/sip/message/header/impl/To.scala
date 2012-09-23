package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ToHeader
import org.sisioh.sip.util.{ParserBase, Encoder, NameValuePairList, DuplicateNameValueList}
import org.sisioh.sip.message.address.impl.{AddressType, DefaultAddress}
import org.sisioh.sip.core.Separators

trait ToParser extends ParserBase {
  //def to : Parser[To] =
}

object To {
  def apply
  (address: DefaultAddress,
   tag: Option[String] = None,
   parameters: NameValuePairList = NameValuePairList()) = new To(address, tag, parameters)

  def fromFrom(from: From) =
    new To(from.address)

  def unapply(to: To): Option[(DefaultAddress, NameValuePairList)] =
    Some(to.address, to.parameters)

  object JsonEncoder extends Encoder[To] {
    def encode(model: To, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(JField("address", parse(model.address.encodeByJson())) ::
        JField("paramters", parse(model.parameters.encodeByJson())) :: Nil)
      builder.append(compact(render(json)))
    }
  }
}

class To
(val address: DefaultAddress,
 val tag: Option[String] = None,
 parametersParam: NameValuePairList = NameValuePairList())
  extends AddressParametersHeader with ToHeader {

  val parameters = tag.map(t => parametersParam.add("tag",t)).getOrElse(parametersParam)

  val headerName = ToHeader.NAME
  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new To(address, tag, _parameters)
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

  def encodeByJson(builder: StringBuilder) = encode(builder, To.JsonEncoder)

  val name = ToHeader.NAME
}
