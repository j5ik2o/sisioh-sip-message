package org.sisioh.sip.util

trait UtilsExt {

  def generateCallIdentifier(address: String): String

  def generateTag(): String

  def generateBranchId(): String

}
