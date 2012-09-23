package org.sisioh.sip.util

import org.sisioh.sip.core.GenericObject
import util.parsing.combinator.RegexParsers

object HostPortDecoder {
  def apply() = new HostPortDecoder()
}

class HostPortDecoder extends Decoder with HostPortParser {

  def decode(source: String): HostPort = decodeTarget(source, hostPort)
}

trait HostPortParser extends ParserBase with HostParser {

  def hostPort: Parser[HostPort] = host ~ opt(COLON ~> rep1(PORT)) ^^ {
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
