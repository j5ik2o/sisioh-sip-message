package org.sisioh.sip.transaction.v2

import org.sisioh.sip.useragent.Dialog
import org.sisioh.sip.message.Request

trait Transaction {

  def dialog: Dialog

  def branchId: String

  def request: Request
}
