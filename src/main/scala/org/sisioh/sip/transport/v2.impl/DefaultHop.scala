package org.sisioh.sip.transport.impl

import org.sisioh.sip.message.address.Hop

object DefaultHop {

  def apply
  (host: String,
   port: Option[Int],
   transport: String,
   isURIRoute: Boolean): DefaultHop =
    new DefaultHop(host, port, transport, isURIRoute)

  def apply
  (host: String,
   port: Option[Int],
   transport: String): DefaultHop =
    new DefaultHop(host, port, transport)

  def apply(hop: String): DefaultHop = {
    val brack = hop.indexOf(']')
    val colon = hop.indexOf(':', brack)
    val slash = hop.indexOf('/', colon)

    if (colon > 0) {
      val r = if (slash > 0) {
        (hop.substring(colon + 1, slash),
          hop.substring(slash + 1))
      } else {
        (hop.substring(colon + 1),
          "UDP")
      }
      val host = hop.substring(0, colon)
      val port = r._1.toInt
      val transport = r._2
      new DefaultHop(host.trim, Some(port), transport.trim)
    } else {
      if (slash > 0) {
        val host = hop.substring(0, slash)
        val transport = hop.substring(slash + 1)
        val port = if (transport.equalsIgnoreCase("TLS")) 5061 else 5060
        new DefaultHop(host.trim, Some(port), transport.trim)
      } else {
        new DefaultHop(hop.trim, Some(5060), "UDP")
      }
    }
  }

  def unapply(defaultHop: DefaultHop): Option[(String, Option[Int], String, Boolean)] =
    Some(defaultHop.host, defaultHop.port, defaultHop.transport, defaultHop.isURIRoute)

}


class DefaultHop
(hostParam: String,
 val port: Option[Int],
 val transport: String,
 val isURIRoute: Boolean = false)
  extends Hop with Serializable {

  def host = {
    if (hostParam.indexOf(":") >= 0 &&
      hostParam.indexOf("[") < 0) {
      "[%s]".format(hostParam)
    } else {
      hostParam
    }
  }

  override def hashCode() =
    31 * host.## + 31 * port.## + 31 * transport.## + 31 * isURIRoute.##

  override def equals(obj: Any) = obj match {
    case that: DefaultHop =>
      host == that.host &&
        port == that.port &&
        transport == that.transport &&
        isURIRoute == that.isURIRoute
    case _ =>
      false
  }

  override def toString = "DefaultHop(%s,%s,%s,%s)".format(host, port, transport, isURIRoute)
}
