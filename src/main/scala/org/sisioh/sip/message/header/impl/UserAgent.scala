package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.UserAgentHeader
import org.sisioh.sip.util.{Decoder, ParserBase, Encoder}

object UserAgentDecoder extends UserAgentDecoder

class UserAgentDecoder extends Decoder with UserAgentParser {
  def decode(source: String) = decodeTarget(source, USER_AGENTWithCrLfOpt)
}

trait UserAgentParser extends ParserBase {
  lazy val USER_AGENTWithCrLfOpt = USER_AGENT <~ opt(CRLF)

  lazy val USER_AGENT: Parser[UserAgent] = "User-Agent" ~> HCOLON ~> rep1sep(serverVal, LWS) ^^ {
    case serverVals =>
      UserAgent(serverVals)
  }
  lazy val serverVal: Parser[ServerVal] = product | comment ^^ { case c => Comment(c)}
  lazy val product: Parser[Product] = token ~ opt(SLASH ~> productVersion) ^^ {
    case f ~ s =>
      Product(f, s)
  }
  lazy val productVersion = token
}

object UserAgent {

  def decode(source: String) = UserAgentDecoder.decode(source)

  object JsonEncoder extends Encoder[UserAgent] {
    def encode(model: UserAgent, builder: StringBuilder) = {
      import net.liftweb.json._
      val list = model.products.map {
        e => JString(e.toString)
      }.toList
      val json = JArray(list)
      builder.append(compact(render(json)))
    }
  }

}

object Product {

  def from(fullName: String) = {
    val sp = fullName.split("/")
    if (sp.size == 2) {
      Product(sp(0), Some(sp(1)))
    } else if (sp.size == 1) {
      Product(sp(0))
    } else {
      throw new IllegalArgumentException
    }
  }

}

trait ServerVal

case class Comment(comment: String) extends ServerVal

case class Product(name: String, version: Option[String] = None) extends ServerVal {
  override def toString = List(Some(name), version).flatten.mkString("/")
}

case class UserAgent(products: List[ServerVal] = List.empty)
  extends SIPHeader with UserAgentHeader {

  val name = UserAgentHeader.NAME
  val headerName = UserAgentHeader.NAME

  def addProduct(product: Product) = {
    UserAgent(
      product :: products
    )
  }

  def encodeByJson(builder: StringBuilder) = encode(builder, UserAgent.JsonEncoder)

  private def encodeProducts(builder: StringBuilder): StringBuilder = {
    builder.append(products.mkString(" "))
  }

  def encodeBody(builder: StringBuilder) = encodeProducts(builder)

}
