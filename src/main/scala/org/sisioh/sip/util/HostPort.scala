package org.sisioh.sip.util

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

import org.sisioh.sip.core.GenericObject
import util.parsing.combinator.RegexParsers

object HostPortDecoder {
  def apply() = new HostPortDecoder()
}

class HostPortDecoder extends Decoder with HostPortParser {

  def decode(source: String): HostPort = decodeTarget(source, hostPort)
}

trait HostPortParser extends ParserBase with HostParser {

  def hostPort: Parser[HostPort] = hostToModel ~ opt(COLON ~> rep1(PORT)) ^^ {
    case host ~ port =>
      new HostPort(host, port.map(_.mkString.toInt))
  }

  //lazy val COLON = ":"
  lazy val PORT = DIGIT

}

/**
 * [[org.sisioh.sip.util.HostPort]]のコンパニオンオブジェクト
 */
object HostPort {

  def apply(host: Host, port: Option[Int]) = new HostPort(host, port)

  def decode(source: String) = HostPortDecoder().decode(source)

  /**
   * Jsonエンコーダー。
   */
  object JsonEncoder extends Encoder[HostPort] {
    def encode(model: HostPort, builder: StringBuilder) = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val json =
        ("host" -> model.host.encode()) ~
          ("port" -> model.port)
      val jsonText = compact(render(json))
      builder.append(jsonText)
    }
  }

}

/**
 * ホストとポートを表す値オブジェクト。
 *
 * @param host [[org.sisioh.sip.util.Host]]
 * @param port ポート
 */
class HostPort(val host: Host, val port: Option[Int]) extends GenericObject {

  val inetAddress = host.inetAddress

  def removePort =
    HostPort(host, None)

  override def toString = encode

  def encode(builder: StringBuilder) =
      builder.append(port.map("%s:%s".format(host.encode(), _)).getOrElse(host.toString))

  override def hashCode() = 31 * host.## + 31 * port.##

  override def equals(obj: Any) = obj match {
    case that : HostPort =>
      host == that.host && port == that.port
    case _ => false
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, HostPort.JsonEncoder)
}
