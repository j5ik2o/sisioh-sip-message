package org.sisioh.sip.transaction.v2

object TransactionStateType extends Enumeration {

  val CALLING = Value(0, "Calling Transaction")

  val TRYING = Value(1, "Trying Transaction")

  val PROCEEDING = Value(2, "Proceeding Transaction")

  val COMPLETED = Value(3, "Completed Transaction")

  val CONFIRMED = Value(4, "Confirmed Transaction")

  val TERMINATED = Value(5, "Terminated Transaction")

}

