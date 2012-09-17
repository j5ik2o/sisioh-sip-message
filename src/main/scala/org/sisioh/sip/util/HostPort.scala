package org.sisioh.sip.util

import org.sisioh.sip.core.GenericObject

/**
 * [[org.sisioh.sip.util.HostPort]]のコンパニオンオブジェクト
 */
object HostPort {

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
case class HostPort(host: Host, port: Option[Int]) extends GenericObject[HostPort] {

  val inetAddress = host.inetAddress

  def removePort =
    HostPort(host, None)

  override def toString = encode

  def encode(builder: StringBuilder) = builder.append(port.map("%s:%s".format(host.encode(), _)).getOrElse(host.toString))
}
