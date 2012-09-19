package org.sisioh.sip.util

import org.sisioh.sip.core.GenericObject
import util.parsing.combinator.RegexParsers

object HostPortDecoder {
  def apply() = new HostPortDecoder()
}

class HostPortDecoder extends HostPortParser {

  def parse(source: String) = parseAll(hostPort, source) match {
    case Success(result, _) => result
    case Failure(msg, _) => throw new ParseException(Some(msg))
    case Error(msg, _) => throw new ParseException(Some(msg))
  }

}

trait HostPortParser extends RegexParsers with HostParser {

  def hostPort: Parser[HostPort] = host ~ opt(COLON ~> PORT) ^^ {
    case host ~ port =>
      new HostPort(host, port.map(_.toInt))
  }

  lazy val COLON = ":"
  lazy val PORT = """[0-9]+""".r

}

/**
 * [[org.sisioh.sip.util.HostPort]]のコンパニオンオブジェクト
 */
object HostPort {

  def apply(host: Host, port: Option[Int]) = new HostPort(host, port)

  def decode(source: String) = HostPortDecoder().parse(source)

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
class HostPort(val host: Host, val port: Option[Int]) extends GenericObject[HostPort] {

  val inetAddress = host.inetAddress

  def removePort =
    HostPort(host, None)

  override def toString = encode

  def encode(builder: StringBuilder) = builder.append(port.map("%s:%s".format(host.encode(), _)).getOrElse(host.toString))
}
