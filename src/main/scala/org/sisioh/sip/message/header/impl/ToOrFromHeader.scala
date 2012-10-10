package org.sisioh.sip.message.header.impl

/*
 * Copyright 2012 Sisioh Project and others. (http://www.sisioh.org/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */

import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl.{DefaultAddress, DefaultAddressJsonDecoder, AddressType}
import org.sisioh.sip.core.Separators
import scala.Some
import net.liftweb.json._
import org.sisioh.sip.message.header.FromHeader
import scala.Some
import scala.Some
import scala.Some

trait ToOrFromParser extends ParserBase with HostParser {

  lazy val toParam = tagParam | genericParam

  lazy val tagParam: Parser[NameValuePair] = "tag" ~ (EQUAL ~> token) ^^ {
    case n ~ v =>
      NameValuePair(Some(n), Some(v))
  }


}

trait ToOrFromHeader extends AddressParametersHeader {

  def tag: Option[String]

  val parameters: NameValuePairList

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
    case that: ToOrFromHeader =>
      address == that.address &&
        parameters == that.parameters &&
        headerName == that.headerName &&
        duplicates == that.duplicates
    case _ => false
  }

  override def toString = encode()

}

trait ToFromJsonFieldNames extends JsonFieldNames {
  val ADDRESS = "address"
}

abstract class ToFromJsonDecoder[T <: ToOrFromHeader] extends JsonDecoder[T] with ToFromJsonFieldNames {

  val headerName: String
  protected def createInstance(address: DefaultAddress, parameters: NameValuePairList): T

  def decode(json: JsonAST.JValue) = {
    requireHeaderName(json, headerName)
    val address = DefaultAddressJsonDecoder.decode(json \ ADDRESS)
    val parameters = NameValuePairListJsonDecoder.decode(json \ PARAMETERS)
    createInstance(address, parameters)
  }

}


abstract class ToFromJsonEncoder[T <: ToOrFromHeader] extends JsonEncoder[T] with ToFromJsonFieldNames {

  def encode(model: T) = {
    JObject(
      getHeaderNameAsJValue(model) ::
        JField(ADDRESS, model.address.encodeAsJValue()) ::
        JField(PARAMETERS, model.parameters.encodeAsJValue()) :: Nil
    )
  }

}

