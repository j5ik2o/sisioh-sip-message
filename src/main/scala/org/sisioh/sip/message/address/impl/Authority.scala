package org.sisioh.sip.message.address.impl

import org.sisioh.sip.util.{Encodable, Encoder, HostPort}
import org.sisioh.sip.core.Separators

object Authority {

  implicit object DefaultAuthorityEncoder extends Encoder[Authority] {
    def encode(model: Authority, builder: StringBuilder) = {
      model.userInfo.map {
        ui =>
          model.hostPort.get.encode(ui.encode(builder).append(Separators.AT))
      }.getOrElse{
        model.hostPort.get.encode(builder)
      }
    }
  }

}

case class Authority(hostPort: Option[HostPort], userInfo: Option[UserInfo]) extends Encodable[Authority] {
  val userName = userInfo.map(_.name)
  val host = hostPort.map(_.host)
  val port: Option[Int] = hostPort.flatMap(_.port)

  def removePort: Authority = Authority(hostPort.map(_.removePort), userInfo)
  def removeUserInfo: Authority = Authority(hostPort, None)
}
