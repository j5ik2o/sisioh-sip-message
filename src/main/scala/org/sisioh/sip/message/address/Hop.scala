package org.sisioh.sip.message.address

trait Hop {

  def host: String

  def port: Option[Int]

  def transport: String

  def toString
}
