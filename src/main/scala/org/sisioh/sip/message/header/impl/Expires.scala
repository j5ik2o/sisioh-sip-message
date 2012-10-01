package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ExpiresHeader
import org.sisioh.sip.util._
import net.liftweb.json._

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

trait ExpiresJsonFieldNames extends JsonFieldNames {

  val EXPIRES = "expires"

}

object ExpiresJsonDecoder extends JsonDecoder[Expires] with ExpiresJsonFieldNames {

  def decode(json: JsonAST.JValue) = {
    requireHeaderName(json, ExpiresHeader.NAME)
    val JInt(expires) = json \ EXPIRES
    Expires(expires.toInt)
  }

}

object ExpiresJsonEncoder extends JsonEncoder[Expires] with ExpiresJsonFieldNames {

  def encode(model: Expires) = {
    JObject(
      getHeaderNameAsJValue(model) ::
        JField(EXPIRES, JInt(BigInt(model.expires))) :: Nil
    )
  }

}


object Expires {

  def decode(source: String) = ExpiresDecoder.decode(source)

  def decodeFromJson(source: String) = ExpiresJsonDecoder.decode(source)

}

case class Expires(expires: Int) extends SIPHeader with ExpiresHeader {
  require(expires > 0)

  val headerName = ExpiresHeader.NAME

  val name = headerName

  def encodeBody(builder: StringBuilder) =
    builder.append(expires)

  def encodeAsJValue() = ExpiresJsonEncoder.encode(this)

}
