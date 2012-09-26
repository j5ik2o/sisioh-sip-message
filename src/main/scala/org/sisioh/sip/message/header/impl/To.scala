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

import org.sisioh.sip.message.header.ToHeader
import org.sisioh.sip.util._
import org.sisioh.sip.message.address.impl._
import org.sisioh.sip.core.Separators
import scala.Some
import org.sisioh.sip.message.address.{URI, SipURI}
import org.sisioh.sip.message.address.impl.AddressType
import scala.Some
import scala.Some

object ToDecoder extends ToDecoder

class ToDecoder extends SIPDecoder[To] with ToParser {
  def decode(source: String):To = decodeTarget(source, toWithCrLfOpt)
}

trait ToParser extends ToOrFromParser with DefaultAddressParser {

  lazy val toWithCrLfOpt: Parser[To] = to <~ opt(CRLF)

  lazy val to: Parser[To] = ("To" | "t") ~> HCOLON ~> ((nameAddrToDefaultAddress | addrSpecToDefaultAddress) ~ rep(SEMI ~> toParam)) ^^ {
    case da ~ toParams =>
      To(da, None, NameValuePairList.fromValues(toParams))
  }

}

/**
 * [[org.sisioh.sip.message.header.impl.To]]のためのコンパニオンオブジェクト。
 */
object To {

  def apply
  (address: DefaultAddress,
   tag: Option[String] = None,
   parameters: NameValuePairList = NameValuePairList()) = new To(address, tag, parameters)

  def fromFrom(from: From) =
    new To(from.address)

  def unapply(to: To): Option[(DefaultAddress, NameValuePairList)] =
    Some(to.address, to.parameters)

  def decode(source: String) = ToDecoder.decode(source)

  object JsonEncoder extends Encoder[To] {
    def encode(model: To, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(
        JField("headerName", JString(model.headerName)) ::
        JField("address", parse(model.address.encodeByJson())) ::
        JField("paramters", parse(model.parameters.encodeByJson())) :: Nil
      )
      builder.append(compact(render(json)))
    }
  }

}

/**
 * Toヘッダを表す値オブジェクト。
 *
 * @param address [[org.sisioh.sip.message.address.impl.DefaultAddress]]
 * @param tag タグ
 * @param parametersParam [[org.sisioh.sip.util.NameValuePairList]]
 */
class To
(val address: DefaultAddress,
 val tag: Option[String] = None,
 parametersParam: NameValuePairList = NameValuePairList())
  extends ToOrFromHeader with ToHeader {

  type ParametersHeaderType = To

  val headerName = ToHeader.NAME

  val parameters = tag.map(t => parametersParam.add("tag", t)).getOrElse(parametersParam)

  val duplicates: DuplicateNameValueList = DuplicateNameValueList()

  protected def createParametersHeader(_duplicates: DuplicateNameValueList, _parameters: NameValuePairList) = {
    new To(address, tag, _parameters)
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, To.JsonEncoder)

}
