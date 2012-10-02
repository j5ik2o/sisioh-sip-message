package org.sisioh.sip.message.header.impl

import org.sisioh.sip.message.header.{SIPConstants, SIPStatusLine}
import org.sisioh.sip.core.Separators
import org.sisioh.sip.util.ParserBase


trait StatusLineParser extends ParserBase {
  lazy val Status_Line = SIP_Version ~ SP ~ Status_Code ~ SP ~ Reason_Phrase ~ CRLF

  lazy val Status_Code = Informational | Redirection | success | Client_Error | Server_Error | Global_Failure | extensionCode

  lazy val extensionCode: Parser[String] = DIGIT ~ DIGIT ~ DIGIT ^^ {
    case f ~ s ~ t => List(f, s, t).mkString
  }

  lazy val Reason_Phrase = rep(reserved | unreserved | escaped | UTF8_NONASCII | UTF8_CONT | SP | HTAB)

  lazy val Informational: Parser[String] = "100" | //  Trying (試行中)
    "180" | //  Ringing (呼び出し中)
    "181" | //  Call Is Being Forwarded (呼が転送されている)
    "182" | // Queued (キューに入れられた)
    "183" // Session Progress (セッションの進捗状況)

  lazy val success: Parser[String] = "200" //  OK

  lazy val Redirection: Parser[String] = "300" | //  Multiple Choices (複数の選択肢がある)
    "301" | //  Moved Permanently (恒久的に移動した)
    "302" | //  Moved Temporarily (一時的に移動した)
    "305" | //  Use Proxy (プロキシを使用せよ)
    "380" //  Alternative Service (代替サービス)

  lazy val Client_Error = "400" | //  Bad Request (不正なリクエスト)
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
    "488" | //  Not Acceptable Here (ここでは受け入れ不能)
    "491" | //  Request Pending (リクエストペンディング)
    "493" //  Undecipherable (解読不能)

  lazy val Server_Error: Parser[String] = "500" | //  Internal Server Error (内部サーバーエラー)[訳注: 21.5.1ではServer Internal Error]
    "501" | //  Not Implemented (実装されていない)
    "502" | //  Bad Gateway (不正なゲートウェイ)
    "503" | //  Service Unavailable (サービスを利用できない)
    "504" | //  Server Time-out (サーバータイムアウト)
    "505" | //  SIP Version not supported (サポートされていないSIPバージョン)[訳注: 21.5.6では「SIP」がない]
    "513" //  Message Too Large (メッセージが大きすぎる)

  lazy val Global_Failure = "600" | //  Busy Everywhere (どの場所もビジー)
    "603" | //  Decline (辞退)
    "604" | //  Does not exist anywhere (どこにも存在しない)
    "606" //  Not Acceptable (受け入れ不能)

}


case class StatusLine
(statusCode: Int,
 reasonPhrase: Option[String],
 sipVersion: Option[String] = Some(SIPConstants.SIP_VERSION_STRING))
  extends SIPObject with SIPStatusLine with VersionSpliter {

  lazy val versionMajor = versionSplis(sipVersion, 0)

  lazy val versionMinor = versionSplis(sipVersion, 1)

  def encode(builder: StringBuilder) = {
    builder.append(sipVersion.getOrElse(SIPConstants.SIP_VERSION_STRING) + Separators.SP + statusCode)
    reasonPhrase.foreach {
      e => builder.append(Separators.SP + e)
    }
    builder.append(Separators.NEWLINE)
    builder
  }

  def encodeAsJValue() = null
}
