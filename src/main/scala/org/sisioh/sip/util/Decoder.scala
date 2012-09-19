package org.sisioh.sip.util

import util.parsing.combinator.RegexParsers

trait Decoder[A] extends RegexParsers {

  protected def decodeTarget(source: String, parser: Parser[A]): A = parseAll[A](parser, source) match {
    case Success(result, _) => result
    case Failure(msg, _) => throw new ParseException(Some(msg))
    case Error(msg, _) => throw new ParseException(Some(msg))
  }

}
