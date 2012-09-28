package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ExpiresHeader
import org.sisioh.sip.util.{SIPDecoder, Decoder, ParserBase, Encoder}

object ExpiresDecoder extends ExpiresDecoder

class ExpiresDecoder extends SIPDecoder[Expires] with ExpiresParser {
  def decode(source: String) = decodeTarget(source, expireWithCrLfOpt)
}

trait ExpiresParser extends ParserBase {
  lazy val expireWithCrLfOpt = expires <~ opt(CRLF)

  lazy val expires: Parser[Expires] = "Expires" ~> HCOLON ~> deltaSeconds ^^ {
    e =>
      Expires(e.toInt)
  }
}

object Expires {

  def decode(source: String) = ExpiresDecoder.decode(source)

  object JsonEncoder extends Encoder[Expires] {
    def encode(model: Expires, builder: StringBuilder) = {
      import net.liftweb.json._
      val json = JObject(
        JField("headerName", JString(model.headerName)) ::
        JField("expires", JInt(BigInt(model.expires))) :: Nil
      )
      builder.append(compact(render(json)))
    }
  }

}

case class Expires(expires: Int) extends SIPHeader with ExpiresHeader {
  require(expires > 0)

  val headerName = ExpiresHeader.NAME

  val name = headerName

  def encodeByJson(builder: StringBuilder) = encode(builder, Expires.JsonEncoder)

  def encodeBody(builder: StringBuilder) =
    builder.append(expires)

  override def toString = encode()

}
