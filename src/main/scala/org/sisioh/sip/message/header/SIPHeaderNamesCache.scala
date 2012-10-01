package org.sisioh.sip.message.header

object SIPHeaderNamesCache {

  private val valuesMap = SIPHeaderNames.values.flatMap {
    e =>
      val lowerCase = e.toLowerCase
      List((e, lowerCase), (lowerCase, lowerCase))
  }.toMap

  def toLowerCase(headerName: String) = {
    val r = valuesMap.get(headerName).getOrElse(headerName.toLowerCase.intern())
    //println("r = "+r)
    r
  }

}
