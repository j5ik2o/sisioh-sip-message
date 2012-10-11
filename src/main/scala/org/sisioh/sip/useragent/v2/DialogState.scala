package org.sisioh.sip.useragent.v2

trait DialogState {

}

object DialogState {
  object EaryDialogState extends DialogState
  object ConfirmedDialogState extends DialogState
  object CompletedDialogState extends DialogState
  object TerminatedDialogState extends DialogState

  val states = List(
    EaryDialogState,
    ConfirmedDialogState,
    CompletedDialogState,
    TerminatedDialogState
  )
  def resolve(dialogStateType: DialogStateType.Value) = {
    states(dialogStateType.id)
  }
}


object DialogStateType extends Enumeration {
  val EARY = Value(0, "Early Dialog")
  val CONFIRMED = Value(1, "Confirmed Dialog")
  val COMPLETED = Value(2, "Completed Dialog")
  val TERMINATED = Value(3, "Terminated Dialog")
}