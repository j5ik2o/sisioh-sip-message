package org.sisioh.sip.util

object ErrorReason extends Enumeration {
  /**
   * トランスポートエラー
   */
  val TRANSPORT,
  /**
   * 不明なエラー
   */
  UNKNOWN,
  /**
   * TimerBのタイムアウト
   */
  TIMEOUT_B,
  /**
   * TimerFのタイムアウト
   */
  TIMEOUT_F,
  /**
   * TimerHのタイムアウト
   */
  TIMEOUT_H,
  /**
   * メッセージが不正
   */
  MESSSAGE,
  /**
   * ノンオペレーション
   */
  NOP = Value
}
