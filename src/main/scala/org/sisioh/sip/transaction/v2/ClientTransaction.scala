package org.sisioh.sip.transaction.v2

import org.sisioh.sip.message.Request

trait ClientTransaction extends Transaction {

  def sendRequest: Unit

  def createCancel: Request

  def createAck: Request

}
