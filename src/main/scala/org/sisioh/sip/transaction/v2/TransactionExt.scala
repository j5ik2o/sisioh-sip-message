package org.sisioh.sip.transaction.v2

import java.security.cert.Certificate

trait TransactionExt extends Transaction {

  def peerAddress: String

  def peerPort: Int

  def transport: String

  def host: String

  def port: Int

  def cipherSuite: String

  def localCertificates: Array[Certificate]

  def peerCertificates: Array[Certificate]

  def extractCertIdentities: List[String]

  def isReleaseReferences: Boolean

  def timerT2: Int

  def timerT4: Int

  def timerD: Int
}
