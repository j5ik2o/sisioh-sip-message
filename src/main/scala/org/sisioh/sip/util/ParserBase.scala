package org.sisioh.sip.util

import util.parsing.combinator.RegexParsers

trait ParserBase extends RegexParsers {

  lazy val ALPHA = """[a-zA-Z]""".r
  lazy val DIGIT = """\d""".r
  lazy val HOSTNAME = """(([a-zA-Z]|([a-zA-Z0-9])([a-zA-Z0-9]|[-])*([a-zA-Z0-9]))[.])*(([a-zA-Z][a-zA-Z0-9]*[a-zA-Z])|[a-zA-Z])[.]?""".r

  def chr(c: Char): Parser[Char] = c

  def chrRange(f: Char, t: Char): Parser[Char] = elem("[]", c => f <= c && c <= t)

  lazy val quotedPair = """\""" ~ (chrRange(0x00, 0x09) | chrRange(0x0B, 0x0C) | chrRange(0x0E, 0x7F)) ^^ {
    case bs ~ c => bs + c.toString
  }

  lazy val token: Parser[String] = rep1(alphanum | "-" | "." | "!" | "%" | "*" | "_" | "+" | "`" | "'" | "~") ^^ {
    _.mkString
  }

  lazy val DQUOTE = "\""

  lazy val qdtext = /*LWS*/ chr(0x21) | chrRange(0x23, 0x5B) | chrRange(0x5D, 0x7E) | UTF8_NONASCII

  lazy val quotedString = /*SWS*/ DQUOTE ~ rep(qdtext | quotedPair) ~ DQUOTE

  lazy val comment: Parser[String] = "(" ~ rep(ctext | quotedPair | comment) ~ ")" ^^ {
    case lb ~ texts ~ rb =>
      lb + texts.mkString + rb
  }

  lazy val UTF8_CONT = chrRange(0x80, 0xBF)

  lazy val UTF8_NONASCII = chrRange(0xC0, 0xDF) ~ UTF8_CONT | chrRange(0xE0, 0xEF) ~ repN(2, UTF8_CONT) |
    chrRange(0xF0, 0xF7) ~ repN(3, UTF8_CONT) | chrRange(0xF8, 0xFb) ~ repN(4, UTF8_CONT) | chrRange(0xFC, 0xFD) ~ repN(5, UTF8_CONT)

  lazy val ctext = chrRange(0x21, 0x27) | chrRange(0x2A, 0x5B) | chrRange(0x5D, 0x7E) | UTF8_NONASCII //| LWS

  lazy val reserved: Parser[String] = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ","

  lazy val alphanum: Parser[String] = (ALPHA | DIGIT)

  lazy val mark: Parser[String] = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"

  lazy val unreserved: Parser[String] = alphanum | mark

  lazy val escaped: Parser[String] = "%" ~> """[0-9A-Fa-f]{0,3}""".r ~ """[0-9A-Fa-f]{0,3}""".r ^^ {
    case f ~ s => f + s
  }

  lazy val Method = INVITEm | ACKm | OPTIONSm | BYEm | CANCELm | REGISTERm | extensionMethod

  val INVITEm = "INVITE"
  val ACKm = "ACK"
  val OPTIONSm = "OPTION"
  val BYEm = "BYE"
  val CANCELm = "CANCEL"
  val REGISTERm = "REGISTER"
  val extensionMethod = token
}
