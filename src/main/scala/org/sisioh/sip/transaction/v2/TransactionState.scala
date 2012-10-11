package org.sisioh.sip.transaction.v2

trait TransactionState {

}


object TransactionState {

  object CallingTransactionState extends TransactionState

  object TryingTransactionState extends TransactionState

  object ProceedingTransactionState extends TransactionState

  object CompletedTransactionState extends TransactionState

  object ConfirmedTransactionState extends TransactionState

  object TerminatedTransactionState extends TransactionState
  val states = List(
    CallingTransactionState, TryingTransactionState,
    ProceedingTransactionState, CompletedTransactionState,
    ConfirmedTransactionState, TerminatedTransactionState
  )

  def resolve(transactionStateType: TransactionStateType.Value): TransactionState = {
    states(transactionStateType.id)
  }

}




