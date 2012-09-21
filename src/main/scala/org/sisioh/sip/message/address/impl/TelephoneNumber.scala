package org.sisioh.sip.message.address.impl

import org.sisioh.sip.util.{NameValuePair, Decoder, ParserBase, NameValuePairList}
import org.sisioh.sip.core.{Separators, GenericObject}

object TelephoneNumberDecoder {
  def apply() = new TelephoneNumberDecoder
}

class TelephoneNumberDecoder extends Decoder with TelephoneNumberParser {
  def decode(source: String): TelephoneNumber = decodeTarget(source, telephoneNumber)
}

trait TelephoneNumberParser extends ParserBase {

  def telephoneNumber: Parser[TelephoneNumber] = telephoneSubscriber

  def telephoneSubscriber: Parser[TelephoneNumber] = globalPhoneNumber | localPhoneNumber

  lazy val localPhoneNumber: Parser[TelephoneNumber] = rep1(phonedigit | dtmfDigit | pauseCharacter) ~ opt(isdnSubAddress) ~
    opt(postDial) ~ areaSpecifier ~ rep(areaSpecifier | serviceProvider | futureExtension) ^^ {
    case d ~ isdnOpt ~ postDialOpt ~ area ~ afterOpts =>
      val list = NameValuePairList.fromValues(afterOpts).add(area)
      val list2 = isdnOpt.map(e => list.add(e)).getOrElse(list)
      val list3 = postDialOpt.map(e => list2.add(e)).getOrElse(list2)
      TelephoneNumber(d.mkString, false , list3)
  }

  lazy val opts: Parser[NameValuePairList] = areaSpecifier ~ serviceProvider ^^ {
    case a ~ s =>
      NameValuePairList.fromValues(List(a, s))
  } | futureExtension ^^ {
    nv => NameValuePairList.fromValues(List(nv))
  }

  lazy val globalPhoneNumber: Parser[TelephoneNumber] = "+" ~ basePhoneNumber ~ opt(isdnSubAddress) ~ opt(postDial) ~
    rep(opts) ^^ {
    case plus ~ baseNumber ~ isdnSubAddrOpt ~ postDialOpt ~ repOpts =>
      val list = repOpts.foldLeft(NameValuePairList())((r, l) => r.add(l))
      val list2 = isdnSubAddrOpt.map(e => list.add(e)).getOrElse(list)
      val list3 = postDialOpt.map(e => list2.add(e)).getOrElse(list2)
      TelephoneNumber(baseNumber, true, list3)
  }

  lazy val CHAR: Parser[Char] = chrRange(0x01, 0x7F)

  lazy val quotedBodyChar: Parser[String] = """\""" ~ CHAR ^^ {
    case f ~ s => f + s
  } | (chrRange(0x20, 0x21) | chrRange(0x23, 0x7E) | chrRange(0x80, 0xFF)) ^^ {
    case c => c.toString
  }

  lazy val quotedString: Parser[String] = chr(0x22) ~ rep(quotedBodyChar) ~ chr(0x22) ^^ {
    case qs ~ bodyChars ~ qe =>
      qs.toString + bodyChars.mkString + qe.toString
  }

  lazy val futureExtension3: Parser[String] = rep1(tokenChar) ~ opt("?" ~ rep1(tokenChar)) ^^ {
    case tokenChars ~ tokenCharsOpt =>
      tokenChars.mkString + tokenCharsOpt.map {
        case q ~ t =>
          q + t.mkString
      }.getOrElse("")
  }

  lazy val futureExtension2: Parser[String] = "=" ~> (futureExtension3 | quotedString)

  lazy val futureExtension: Parser[NameValuePair] = ";" ~> rep1(tokenChar) ~ opt(futureExtension2) ^^ {
    case tokenChars ~ futureExtension2Opt =>
      NameValuePair(Some(tokenChars.mkString), futureExtension2Opt)
  }

  lazy val tokenChar = (chr(0x21) | chrRange(0x23, 0x27) | chrRange(0x2A, 0x2B) | chrRange(0x2D, 0x2E) | chrRange(0x30, 0x39) | chrRange(0x41, 0x5A) | chrRange(0x5E, 0x7A) | chr(0x7C) | chr(0x7E))

  lazy val serviceProvider: Parser[NameValuePair] = ";" ~> providerTag ~ ("=" ~> providerHostname) ^^ {
    case pt ~ ph =>
      NameValuePair(Some(pt), Some(ph))
  }

  lazy val providerTag = "tsp"

  lazy val providerHostname: Parser[String] = HOSTNAME

  lazy val areaSpecifier: Parser[NameValuePair] = ";" ~> phoneContextTag ~ ("=" ~> phoneContextIdent) ^^ {
    case pct ~ pci =>
      NameValuePair(Some(pct), Some(pci))
  }

  lazy val phoneContextTag = "phone-context"

  lazy val phoneContextIdent: Parser[String] = networkPrefix | privatePrefix

  lazy val privatePrefix: Parser[String] = (chrRange(0x21, 0x22) | chrRange(0x24, 0x27) | chr(0x2C) | chr(0x2F) | chr(0x3A) |
    chrRange(0x3C, 0x40) | chrRange(0x45, 0x4F) | chrRange(0x51, 0x56) | chrRange(0x58, 0x60) |
    chrRange(0x65, 0x6F) | chrRange(0x71, 0x76) | (chrRange(0x78, 0x7E))) ~ rep(chrRange(0x21, 0x3A) | chrRange(0x3C, 0x7E)) ^^ {
    case f ~ s => f.toString + s.mkString
  }

  lazy val networkPrefix: Parser[String] = globalNetworkPrefix | localNetworkPrefix

  lazy val globalNetworkPrefix: Parser[String] = "+" ~ rep1(phonedigit) ^^ {
    case f ~ s =>
      f + s.mkString
  }

  lazy val localNetworkPrefix: Parser[String] = rep1(phonedigit | dtmfDigit | pauseCharacter) ^^ {
    _.mkString
  }

  lazy val postDial: Parser[NameValuePair] = ";postd=" ~> rep1(phonedigit | dtmfDigit | pauseCharacter) ^^ {
    phoneDigits => NameValuePair(Some(ParameterNames.POSTDIAL), Some(phoneDigits.mkString))
  }

  lazy val dtmfDigit = "*" | "#" | "A" | "B" | "C" | "D"

  lazy val pauseCharacter = oneSecondPause | waitForDialTone

  lazy val oneSecondPause = "p"

  lazy val waitForDialTone = "w"

  lazy val isdnSubAddress: Parser[NameValuePair] = ";isub=" ~> rep1(phonedigit) ^^ {
    phoneDigits => NameValuePair(Some(ParameterNames.ISUB), Some(phoneDigits.mkString))
  }

  lazy val basePhoneNumber = rep1(phonedigit) ^^ {
    _.mkString
  }

  lazy val phonedigit = DIGIT | visualSeparator

  lazy val visualSeparator = "-" | "." | "(" | ")"
}


case class TelephoneNumber
(phoneNumber: String,
 isGlobal: Boolean = true,
 params: NameValuePairList = NameValuePairList()) extends GenericObject {

  lazy val postDial = params.getValue(ParameterNames.POSTDIAL).map(_.asInstanceOf[String])
  lazy val hasPostDial = postDial.isDefined
  lazy val isdnSubaddress = params.getValue(ParameterNames.ISUB).map(_.asInstanceOf[String])
  lazy val hasIsdnSubAddress = hasParams(ParameterNames.ISUB)

  def hasParams(name: String) = params.hasNameValuePair(name)


  def removePostDial =
    removeParameter(ParameterNames.POSTDIAL)

  def removeIsdnSubaddress =
    removeParameter(ParameterNames.ISUB)

  def withPostDial(postDial: String) = withParameter(ParameterNames.POSTDIAL, postDial)

  def withIsdnSubaddress(subAddress: String) = withParameter(ParameterNames.ISUB, subAddress)

  def withParameter(name: String, value: Any) =
    TelephoneNumber(
      phoneNumber,
      isGlobal,
      params.add(name, value)
    )

  def getParameter(name: String) = {
    params.getValue(name).map {
      case any: GenericObject =>
        any.encode()
      case other =>
        other.toString
    }
  }

  lazy val parameterNames = params.names

  def removeParameter(name: String) = {
    new TelephoneNumber(
      phoneNumber,
      isGlobal,
      params.remove(name)
    )
  }

  def encode(builder: StringBuilder) = {
    if (isGlobal) {
      builder.append('+')
    }
    builder.append(phoneNumber);
    if (!params.isEmpty) {
      builder.append(Separators.SEMICOLON);
      params.encode(builder);
    }
    builder
  }
}
