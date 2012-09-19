package org.sisioh.sip.message.address


/**
 * アドレスを表す値オブジェクト。
 */
trait Address extends Serializable {

  /**
   * 表示名。
   */
  val displayName: Option[String]
  /**
   * URI。
   */
  val uri: URI
  /**
   * ワイルドカードフラグ。
   */
  val isWildcard: Boolean

}




