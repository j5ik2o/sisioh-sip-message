package org.sisioh.sip.util

import util.parsing.combinator.RegexParsers

trait ParserBase extends RegexParsers {

  lazy val ALPHA = """[a-zA-Z]""".r
  lazy val DIGIT = """\d""".r
  lazy val HOSTNAME = """(([a-zA-Z]|([a-zA-Z0-9])([a-zA-Z0-9]|[-])*([a-zA-Z0-9]))[.])*(([a-zA-Z][a-zA-Z0-9]*[a-zA-Z])|[a-zA-Z])[.]?""".r

  def chr(c: Char): Parser[Char] = c
  def chrRange(f: Char, t: Char): Parser[Char] = elem("[]", c => f <= c && c <= t)


  def token: Parser[String] = alphanum | "-" | "." | "!" | "%" | "*" | "_" | "+" | "`" | "'" | "~"

  def reserved: Parser[String] = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ","

  def alphanum: Parser[String] = (ALPHA | DIGIT)

  def mark: Parser[String] = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"

  def unreserved: Parser[String] = alphanum | mark

  def escaped: Parser[String] = "%" ~> """[0-9A-Fa-f]{0,3}""".r ~ """[0-9A-Fa-f]{0,3}""".r ^^ {
    case f ~ s => f + s
  }

  def Method = INVITEm | ACKm | OPTIONSm | BYEm | CANCELm | REGISTERm | extensionMethod

  val INVITEm = "INVITE"
  val ACKm = "ACK"
  val OPTIONSm = "OPTION"
  val BYEm = "BYE"
  val CANCELm = "CANCEL"
  val REGISTERm = "REGISTER"
  val extensionMethod = token
}
