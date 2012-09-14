package org.sisioh.sip.util

/**
 * [[org.sisioh.sip.util.HostPort]]のコンパニオンオブジェクト
 */
object HostPort {

  /**
   * デフォルトエンコーダー。
   */
  implicit object DefaultHostPortEncoder extends Encoder[HostPort] {
    def encode(model: HostPort, builder: StringBuilder) = {
      builder.append(model.host.encode)
      if (model.port.isDefined){
        builder.append(":").append(model.port.get)
      }
      builder
    }
  }

}

/**
 * ホストとポートを表す値オブジェクト。
 *
 * @param host [[org.sisioh.sip.util.Host]]
 * @param port ポート
 */
case class HostPort(host: Host, port: Option[Int]) extends Encodable[HostPort] {

  val inetAddress = host.inetAddress

  def removePort =
    HostPort(host, None)

}
