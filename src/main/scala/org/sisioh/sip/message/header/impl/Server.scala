package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.ServerHeader
import org.sisioh.sip.util.{Encoder, Decoder, ParserBase}

object ServerDecoder extends ServerDecoder

class ServerDecoder extends Decoder with ServerParser {
  def decode(source: String) = decodeTarget(source, SERVER_WithCrLfOpt)
}

trait ServerParser extends ParserBase with UserAgentParser {
  lazy val SERVER_WithCrLfOpt = SERVER <~ opt(CRLF)

  lazy val SERVER: Parser[Server] = "Server" ~> (HCOLON ~> rep1sep(serverVal, LWS)) ^^ {
    case serverVals =>
      Server(serverVals)
  }

}

object Server {

  def decode(source: String) = ServerDecoder.decode(source)

  object JsonEncoder extends Encoder[Server] {
    def encode(model: Server, builder: StringBuilder) = {
      import net.liftweb.json._
      val list = model.serverVals.map {
        e => JString(e.toString)
      }.toList
      val json = JArray(list)
      builder.append(compact(render(json)))
    }
  }

}

/**
 * Serverヘッダを表す値オブジェクト
 *
 * @param serverVals [[org.sisioh.sip.message.header.impl.ServerVal]]のリスト
 */
case class Server(serverVals: List[ServerVal] = List.empty)
  extends SIPHeader with ServerHeader {

  val name = ServerHeader.NAME
  val headerName = ServerHeader.NAME

  def encodeByJson(builder: StringBuilder) = encode(builder, Server.JsonEncoder)

  def addProduct(product: Product): Server = {
    Server(
      product :: serverVals
    )
  }

  private def encodeProducts(builder: StringBuilder): StringBuilder =
    builder.append(serverVals.mkString(" "))

  def encodeBody(builder: StringBuilder) = encodeProducts(builder)

  override def toString = encode()

}
