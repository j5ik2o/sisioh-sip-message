package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.{SIPConstants, SIPRequestLine}
import org.sisioh.sip.message.address.impl.{DefaultGenericURIJsonDecoder, SipUriParser, DefaultGenericURI, GenericURI}
import org.sisioh.sip.core.Separators
import org.sisioh.sip.util._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import scala.Some

object RequestLineDecoder extends RequestLineDecoder

class RequestLineDecoder extends SIPDecoder[RequestLine] with RequestLineParser {
  def decode(source: String) = decodeTarget(source, Request_Line)
}

trait RequestLineParser extends ParserBase with SipUriParser {

  lazy val Request_Line = Method ~ (SP ~> Request_URI) ~ (SP ~> SIP_Version <~ CRLF) ^^ {
    case m ~ uri ~ sipVersion =>
      RequestLine(uri, Some(m), Some(sipVersion))
  }

  lazy val Request_URI: Parser[GenericURI] = SIP_URI | SIPS_URI | absoluteURI

  lazy val SIP_Version: Parser[String] = "SIP" ~ "/" ~ rep1(DIGIT) ~ "." ~ rep1(DIGIT) ^^ {
    case sip ~ slash ~ major ~ dot ~ minar =>
      List(sip, slash, major.mkString, dot, minar.mkString).mkString
  }

}


object RequestLineEncoder extends SIPEncoder[RequestLine] {

  def encode(model: RequestLine, builder: StringBuilder) = {
    model.method.foreach {
      m =>
        builder.append(m).append(Separators.SP)
    }
    model.uri.encode(builder).append(Separators.SP)
    model.sipVersion.foreach {
      s =>
        builder.append(s)
    }
    builder.append(Separators.NEWLINE)
    builder
  }

}

object RequestLineJsonDecoder extends JsonDecoder[RequestLine] {

  def decode(json: JsonAST.JValue) = {
    val uriString = (json \ "uri")
    val uri = DefaultGenericURIJsonDecoder.decode(uriString)
    val JString(method) = (json \ "method")
    val sipVersion = (json \ "sipVersion").toOpt.map {
      _.asInstanceOf[JString].s
    }
    RequestLine(uri, Some(method), sipVersion)
  }

}

object RequestLineJsonEncoder extends JsonEncoder[RequestLine] {

  def encode(model: RequestLine) =
    ("uri" -> model.uri.encodeAsJValue()) ~
      ("method" -> model.method) ~
      ("sipVersion" -> model.sipVersion)

}

object RequestLine {

  def decode(source: String) = RequestLineDecoder.decode(source)

  def decodeFromJson(source: String) = RequestLineJsonDecoder.decode(source)

}

case class RequestLine
(uri: GenericURI,
 method: Option[String] = None,
 sipVersion: Option[String] = Some(SIPConstants.SIP_VERSION_STRING))
  extends SIPObject with SIPRequestLine {

  private def versionSplis(index: Int) = {
    sipVersion.flatMap {
      e =>
        val versions = e.split('/').toList match {
          case List(l) => Some(l)
          case List(_, l) => Some(l)
          case _ => None
        }
        versions.flatMap {
          v =>
            v.toArray.toList match {
              case List(major, '.', _) if (index == 0) =>
                Some(major.toString)
              case List(_, '.', minar) if (index == 1) =>
                Some(minar.toString)
              case _ => None
            }
        }
    }
  }

  lazy val versionMajor = versionSplis(0)

  lazy val versionMinor = versionSplis(1)

  def encode(builder: StringBuilder) = RequestLineEncoder.encode(this, builder)

  def encodeAsJValue() = RequestLineJsonEncoder.encode(this)
}
