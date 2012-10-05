package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.{SIPConstants, SIPStatusLine}
import org.sisioh.sip.core.Separators
import org.sisioh.sip.util.{JsonDecoder, JsonEncoder, SIPDecoder, ParserBase}
import org.sisioh.dddbase.core.ValueObjectBuilder
import org.sisioh.sip.message.impl.SIPResponse
import net.liftweb.json.JsonAST.JObject
import scala.Some
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonAST.JInt
import org.sisioh.sip.message.StatusCode

object StatusLineBuilder {
  def apply() = new StatusLineBuilder
}

class StatusLineBuilder extends ValueObjectBuilder[StatusLine, StatusLineBuilder] {

  private var statusCode: StatusCode.Value = _

  private var reasonPhrase: Option[String] = None

  private var sipVersion: Option[String] = None

  def withStatusCode(statusCode: StatusCode.Value) = {
    addConfigurator {
      _.statusCode = statusCode
    }
    getThis
  }

  def withReasonPhrase(reasonPhrase: Option[String]) = {
    addConfigurator {
      _.reasonPhrase = reasonPhrase
    }
    getThis
  }

  def withSipVersion(sipVersion: Option[String]) = {
    addConfigurator {
      _.sipVersion = sipVersion
    }
    getThis
  }

  protected def getThis = this

  protected def newInstance = new StatusLineBuilder

  protected def apply(vo: StatusLine, builder: StatusLineBuilder) {}

  protected def createValueObject = new StatusLine(statusCode, reasonPhrase, sipVersion)
}

object StatusLineDecoder extends StatusLineDecoder

class StatusLineDecoder extends SIPDecoder[StatusLine] with StatusLineParser {

  def decode(source: String) = decodeTarget(source, Status_Line_WithCrLfOpt)

}


trait StatusLineParser extends ParserBase {

  lazy val Status_Line_WithCrLfOpt = Status_Line <~ opt(CRLF)

  lazy val Status_Line: Parser[StatusLine] = SIP_Version ~ (SP ~> Status_Code) ~ (SP ~> Reason_Phrase) ^^ {
    case sipVersion ~ statusCode ~ reasonPhrase =>
      StatusLine(StatusCode(statusCode), Some(reasonPhrase), Some(sipVersion))
  }

  lazy val Status_Code: Parser[Int] = Informational | Redirection | success | Client_Error | Server_Error | Global_Failure | extensionCode

  lazy val extensionCode: Parser[Int] = DIGIT ~ DIGIT ~ DIGIT ^^ {
    case f ~ s ~ t => List(f, s, t).mkString.toInt
  }

  lazy val Reason_Phrase = rep(reserved | unreserved | escaped | UTF8_NONASCII | UTF8_CONT | SP | HTAB) ^^ {
    _.mkString
  }

  lazy val Informational: Parser[Int] = ("100" | "180" | "181" | "182" | "183") ^^ {
    // 100 Trying (試行中)
    // 180 Ringing (呼び出し中)
    // 181 Call Is Being Forwarded (呼が転送されている)
    // 182 Queued (キューに入れられた)
    // 183 Session Progress (セッションの進捗状況)
    _.toInt
  }

  lazy val success: Parser[Int] = "200" ^^ {
    // 200 OK
    _.toInt
  }

  lazy val Redirection: Parser[Int] = ("300" | "301" | "302" | "305" | "380") ^^ {
    // 300 Multiple Choices (複数の選択肢がある)
    // 301 Moved Permanently (恒久的に移動した)
    // 302 Moved Temporarily (一時的に移動した)
    // 305 Use Proxy (プロキシを使用せよ)
    // 380 Alternative Service (代替サービス)
    _.toInt
  }


  lazy val Client_Error: Parser[Int] = ("400" | //  Bad Request (不正なリクエスト)
    "401" | //  Unauthorized (認証されていない)
    "402" | //  Payment Required (料金支払いが必要)
    "403" | //  Forbidden (禁止)
    "404" | //  Not Found (見つからない)
    "405" | //  Method Not Allowed (メソッドが許可されていない)
    "406" | //  Not Acceptable (受け入れできない)
    "407" | //  Proxy Authentication Required (プロキシ認証が必要)
    "408" | //  Request Timeout (リクエストがタイムアウトした)
    "410" | //  Gone (リソースがもう存在しない)
    "413" | //  Request Entity Too Large (リクエストのエンティティが大きすぎる)
    "414" | //  Request-URI Too Large (Request-URIが長すぎる)
    "415" | //  Unsupported Media Type (サポートされていないメディアタイプ)
    "416" | //  Unsupported URI Scheme (サポートされていないURIスキーム)
    "420" | //  Bad Extension (不正な拡張)
    "421" | //  Extension Required (拡張が必要)
    "423" | //  Interval Too Brief (間隔が短すぎる)
    "480" | //  Temporarily not available (一時的に利用不可)
    "481" | //  Call Leg/Transaction Does Not Exist (コールレグまたはトランザクションが存在しない)[訳注: 21.4.19では「Leg」がない]
    "482" | //  Loop Detected (ループが検知された)
    "483" | //  Too Many Hops (ホップが多すぎる)
    "484" | //  Address Incomplete (アドレスが不完全)
    "485" | //  Ambiguous (不明瞭)
    "486" | //  Busy Here (ここはビジー)
    "487" | //  Request Terminated (リクエストが終了させられた)
    "488" |
    "491" |
    "493") ^^ {
    // 488 Not Acceptable Here (ここでは受け入れ不能)
    // 491 Request Pending (リクエストペンディング)
    // 493 Undecipherable (解読不能)
    _.toInt
  }

  lazy val Server_Error: Parser[Int] = ("500" | //  Internal Server Error (内部サーバーエラー)[訳注: 21.5.1ではServer Internal Error]
    "501" | //  Not Implemented (実装されていない)
    "502" | //  Bad Gateway (不正なゲートウェイ)
    "503" | //  Service Unavailable (サービスを利用できない)
    "504" | //  Server Time-out (サーバータイムアウト)
    "505" | //  SIP Version not supported (サポートされていないSIPバージョン)[訳注: 21.5.6では「SIP」がない]
    "513") ^^ {
    // 513 Message Too Large (メッセージが大きすぎる)
    _.toInt
  }

  lazy val Global_Failure: Parser[Int] = ("600" | //  Busy Everywhere (どの場所もビジー)
    "603" | //  Decline (辞退)
    "604" | //  Does not exist anywhere (どこにも存在しない)
    "606") ^^ {
    // 606 Not Acceptable (受け入れ不能)
    _.toInt
  }

}

trait StatusLineJsonFieldNames extends JsonFieldNames {
  val STATUS_CODE = "statusCode"
  val REASON_PHRASE = "reasonPhrase"
  val SIP_VERSION = "sipVersion"
}

object StatusLineJsonDecoder extends JsonDecoder[StatusLine] with StatusLineJsonFieldNames {

  def decode(json: JValue) = {
    val JInt(statusCode) = json \ STATUS_CODE
    val reasonPhrase = (json \ REASON_PHRASE).toOpt.map {
      _.asInstanceOf[JString].s
    }
    val sipVersion = (json \ SIP_VERSION).toOpt.map {
      _.asInstanceOf[JString].s
    }
    StatusLine(StatusCode(statusCode.toInt), reasonPhrase, sipVersion)
  }

}

object StatusLineJsonEncoder extends JsonEncoder[StatusLine] with StatusLineJsonFieldNames {

  def encode(model: StatusLine) = {
    val statusCode = Some(JField(STATUS_CODE, JInt(BigInt(model.statusCode.id))))
    val reasonPhrase = model.reasonPhrase.map {
      e =>
        JField(REASON_PHRASE, JString(e))
    }
    val sipVersion = model.sipVersion.map {
      e =>
        JField(SIP_VERSION, JString(e))
    }.orElse(Some(JField(SIP_VERSION, JString(SIPConstants.SIP_VERSION_STRING))))

    val fields: List[JField] = List(statusCode, reasonPhrase, sipVersion).flatten
    JObject(fields)
  }

}

object StatusLine {

  def apply
  (statusCode: StatusCode.Value,
   reasonPhrase: Option[String] = None,
   sipVersion: Option[String] = Some(SIPConstants.SIP_VERSION_STRING)) =
    new StatusLine(statusCode, reasonPhrase, sipVersion)

  def unapply(statusLine: StatusLine): Option[(StatusCode.Value, Option[String], Option[String])] =
    Some(statusLine.statusCode, statusLine.reasonPhrase, statusLine.sipVersion)

  def decode(source: String) = StatusLineDecoder.decode(source)

}


class StatusLine
(val statusCode: StatusCode.Value,
 reasonPhraseParam: Option[String] = None,
 val sipVersion: Option[String] = Some(SIPConstants.SIP_VERSION_STRING))
  extends SIPObject with SIPStatusLine with VersionSpliter {

  val reasonPhrase = reasonPhraseParam.orElse(SIPResponse.getReasonPhrase(statusCode.id))

  lazy val versionMajor = versionSplis(sipVersion, 0)

  lazy val versionMinor = versionSplis(sipVersion, 1)

  def encode(builder: StringBuilder) = {
    builder.append(sipVersion.getOrElse(SIPConstants.SIP_VERSION_STRING) + Separators.SP + statusCode.id)
    reasonPhrase.foreach {
      e => builder.append(Separators.SP + e)
    }
    builder.append(Separators.NEWLINE)
    builder
  }

  def encodeAsJValue() = StatusLineJsonEncoder.encode(this)

  override def hashCode() = 31 * statusCode.## + 31 * reasonPhrase.## + 31 * sipVersion.##

  override def equals(obj: Any) = obj match {
    case that: StatusLine =>
      statusCode == that.statusCode &&
        reasonPhrase == that.reasonPhrase &&
        sipVersion == that.sipVersion
    case _ =>
      false
  }
}
