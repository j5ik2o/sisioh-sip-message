package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ServerHeader
import org.sisioh.sip.util._
import net.liftweb.json._


object ServerDecoder extends ServerDecoder

class ServerDecoder extends SIPDecoder[Server] with ServerParser {
  def decode(source: String) = decodeTarget(source, SERVER_WithCrLfOpt)
}

trait ServerParser extends ParserBase with UserAgentParser {
  lazy val SERVER_WithCrLfOpt = SERVER <~ opt(CRLF)

  lazy val SERVER: Parser[Server] = "Server" ~> (HCOLON ~> rep1sep(serverVal, LWS)) ^^ {
    case serverVals =>
      Server(serverVals)
  }

}

object ServerJsonDecoder extends JsonDecoder[Server] with UserAgentJsonDecoderSupport {

  def decode(json: JsonAST.JValue) = {
    requireHeaderName(json, ServerHeader.NAME)
    Server(decodeServerVals(json))
  }

}

object ServerJsonEncoder extends JsonEncoder[Server] with UserAgentJsonEncoderSupport {

  def encode(model: Server) = {
    JObject(
      getHeaderNameAsJValue(model) ::
        encodeServerVals(model.serverVals) ::
        Nil
    )
  }

}

object Server {

  def decode(source: String) = ServerDecoder.decode(source)

  def decodeFromJson(source: String) = ServerJsonDecoder.decode(source)

}

/**
 * Serverヘッダを表す値オブジェクト
 *
 * @param serverVals [[org.sisioh.sip.message.header.impl.ServerVal]]のリスト
 */
case class Server(serverVals: List[ServerVal] = List.empty)
  extends SIPHeader with ServerHeader {

  val headerName = ServerHeader.NAME
  val name = headerName

  def addProduct(product: Product): Server = {
    Server(
      product :: serverVals
    )
  }

  private def encodeProducts(builder: StringBuilder): StringBuilder =
    builder.append(serverVals.mkString(" "))

  def encodeBody(builder: StringBuilder) = encodeProducts(builder)

  def encodeAsJValue() = ServerJsonEncoder.encode(this)

  override def toString = encode()
}
