package org.sisioh.sip.util

case class ParseException(message: Option[String] = None, cause: Option[Throwable] = None)
  extends Exception(message.getOrElse(""), cause.orNull)
