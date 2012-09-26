package org.sisioh.sip.message.header.impl

import org.sisioh.sip.util.SIPHeaderListMappingUtil


object SIPHeaderListMapping {

  private val headers: Map[Class[_], Class[_]] =
    Map(
      classOf[Via] -> classOf[ViaList]
    )

  def hasList(sipHeader: SIPHeader) = {
    sipHeader match {
      case sh: SIPHeaderList[_, _] => false
      case _ =>
        val clazz = sipHeader.getClass
        headers.contains(clazz)
    }
  }

  def hasList(sipHeaderClass: Class[_]) = {
    headers.contains(sipHeaderClass)
  }

  def getListClass(sipHeaderClass: Class[_]) = {
    headers.get(sipHeaderClass)
  }

  def getList(sipHeader: SIPHeader) = {
    SIPHeaderListMappingUtil.getList(sipHeader)
  }


}
