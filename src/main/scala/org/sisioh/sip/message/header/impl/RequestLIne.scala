package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.{SIPConstants, SIPRequestLine}
import org.sisioh.sip.message.address.impl.{DefaultGenericURIJsonDecoder, SipUriParser, DefaultGenericURI, GenericURI}
import org.sisioh.sip.core.Separators
import org.sisioh.sip.util._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import scala.Some
import org.sisioh.dddbase.core.ValueObjectBuilder

object RequestLineBuilder {

  def apply() = new RequestLineBuilder

}

class RequestLineBuilder extends ValueObjectBuilder[RequestLine, RequestLineBuilder] {

  protected def getThis = this

  protected def newInstance = new RequestLineBuilder

  protected def apply(vo: RequestLine, builder: RequestLineBuilder) {
    builder.withUri(vo.uri)
    builder.withMethod(vo.method)
    builder.withSipVersion(vo.sipVersion)
  }

  private var uri: GenericURI = _

  private var method: Option[String] = None

  private var sipVersion: Option[String] = None

  def withUri(uri: GenericURI) = {
    addConfigurator {
      _.uri = uri
    }
    getThis
  }

  def withMethod(method: Option[String]) = {
    addConfigurator {
      _.method = method
    }
    getThis
  }

  def withSipVersion(sipVersion: Option[String]) = {
    addConfigurator {
      _.sipVersion = sipVersion
    }
    getThis
  }


  protected def createValueObject = RequestLine(uri, method, sipVersion)

}

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
  extends SIPObject with SIPRequestLine with VersionSpliter {

  lazy val versionMajor = versionSplis(sipVersion, 0)

  lazy val versionMinor = versionSplis(sipVersion, 1)

  def encode(builder: StringBuilder) = RequestLineEncoder.encode(this, builder)

  def encodeAsJValue() = RequestLineJsonEncoder.encode(this)
}
