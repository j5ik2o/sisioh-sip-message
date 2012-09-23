package org.sisioh.sip.util

import org.specs2.mutable.Specification

object BaseParser extends BaseParser

class BaseParser extends ParserBase with Decoder

class ParserBaseSpec extends Specification {
  "ALPHA_SMALL" in {
    ('a' to 'z').map {
      az =>
        BaseParser.decodeTarget(az.toString, BaseParser.ALPHA_SMALL) must_== az
    }
  }
  "ALPHA_LARGE" in {
    ('A' to 'Z').map {
      az =>
        BaseParser.decodeTarget(az.toString, BaseParser.ALPHA_LARGE) must_== az
    }
  }
  "ALPHA" in {
    ('a' to 'z').map {
      az =>
        BaseParser.decodeTarget(az.toString, BaseParser.ALPHA) must_== az
    }
    ('A' to 'Z').map {
      az =>
        BaseParser.decodeTarget(az.toString, BaseParser.ALPHA) must_== az
    }
  }
  "DIGIT" in {
    ('0' to '9').map {
      d =>
        BaseParser.decodeTarget(d.toString, BaseParser.DIGIT) must_== d
    }
  }
  "CR" in {
    val cr = '\r'
    BaseParser.decodeTarget(cr.toString, BaseParser.CR) must_== cr
  }
  "LF" in {
    val lf = '\n'
    BaseParser.decodeTarget(lf.toString, BaseParser.LF) must_== lf
  }
  "CRLF" in {
    val crlf = "\r\n"
    BaseParser.decodeTarget(crlf, BaseParser.CRLF) must_== crlf
  }
  "SP" in {
    val sp = ' '
    BaseParser.decodeTarget(sp.toString, BaseParser.SP) must_== sp
  }
  "HT" in {
    val ht = '\t'
    BaseParser.decodeTarget(ht.toString, BaseParser.HT) must_== ht
  }
  "CHAR" in {
    (Char.MinValue to Char.MinValue).map {
      c =>
        BaseParser.decodeTarget(c.toString, BaseParser.CHAR) must_== c
    }
  }
  "escaped" in {
    val escaped = "%40"
    BaseParser.decodeTarget(escaped, BaseParser.escaped) must_== 0x40.toChar
  }
  "comment" in {
    val comment = "(abc)"
    BaseParser.decodeTarget(comment, BaseParser.comment) must_== comment
  }

  "LWS" in {
    BaseParser.decodeTarget(" \n ", BaseParser.LWS) must_== " \n "
  }
}
