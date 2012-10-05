package org.sisioh.sip.util

import net.liftweb.json
import json.JsonAST
import org.sisioh.sip.message.header.impl.JsonFieldNames
import org.sisioh.sip.message.header.ViaHeader

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


trait Decoder[A] {

  def decode(source: String): A

}

trait JsonDecoder[A] extends Decoder[A] with JsonFieldNames {

  import net.liftweb.json._

  def decode(source: String) = decode(parse(source))

  def decode(json: JsonAST.JValue): A


  protected def requireHeaderName(json: JValue, headerName: String): Unit =
    require(headerName == getHeaderNameAsString(json))

  protected def getHeaderNameAsJString(json: JValue): JString =
    (json \ HEADER_NAME).asInstanceOf[JString]

  protected def getHeaderNameAsString(json: JValue): String =
    getHeaderNameAsJString(json).s

  protected def getParameters(json: JValue): JArray =
    (json \ PARAMETERS).asInstanceOf[JArray]

  protected def getNameValuePairList(json: JValue) =
    NameValuePairListJsonDecoder.decode(json \ PARAMETERS)


}

trait SIPDecoder[A] extends Decoder[A] with ParserBase {

  def decodeTarget[T](source: String, parser: Parser[T]): T = parseAll[T](parser, source) match {
    case Success(result, _) => result
    case Failure(msg, next) => throw new ParseException(Some(msg+":" + next.first))
    case Error(msg, _) => throw new ParseException(Some(msg))
  }

}
