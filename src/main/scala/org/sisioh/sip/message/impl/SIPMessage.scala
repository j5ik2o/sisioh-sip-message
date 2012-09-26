package org.sisioh.sip.message.impl

/*
 * Copyright 2012 Sisioh Project and others. (http://www.sisioh.org/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */

import org.sisioh.sip.message.header._
import impl._
import org.sisioh.sip.message.Message
import java.net.InetAddress
import org.sisioh.sip.util.SIPHeaderListMappingUtil
import collection.parallel.mutable

trait SIPMessage[T] extends MessageObject with Message[T] with MessageExt {

  val unrecognizedHeaders: List[String]
  val headers: List[SIPHeader]
  val fromHeader: From
  val toHeader: To
  val cSeqHeader: CSeq
  val callIdHeader: CallID
  val contentLengthHeader: ContentLength
  val maxForwards: MaxForwards
  val size: Int
  val messageContent: String
  val messageContentBytes: Array[Byte]
  val messageContentObject: Any
  val headerTable: Map[String, SIPHeader]
  val applicationData: Any
  val forkId: String
  val remoteAddress: InetAddress
  val remotePort: Int
  val localAddress: InetAddress
  val localPort: Int

//  def attachHeader(header: SIPHeader, replace: Boolean, top: Boolean) = {
//    val sipHeader = if (SIPHeaderListMapping.hasList(header) && classOf[SIPHeaderList].isAssignableFrom(header.getClass)) {
//      SIPHeaderListMappingUtil.getList(sipHeader).addLast(sipHeader)
//    } else {
//      header
//    }
//    val headerNameLowerCase = sipHeader.name.toLowerCase
//    if (replace) {
//      headerTable -= headerNameLowerCase
//    } else if (headerTable.contains(headerNameLowerCase) && sipHeader.isInstanceOf[SIPHeaderList] == false) {
//      if (sipHeader.isInstanceOf[ContentLength]) {
//        val cl = sipHeader.asInstanceOf[ContentLength]
//        // ContentLength(cl.contentLength)
//      }
//    }
//    val originalHeader = getHeader(header.name).map(_.asInstanceOf[SIPHeader])
//    if (originalHeader.isDefined) {
//      val li = headers.iterator
//      // li.filterNot(_ == originalHeader.get)
//    }
//    if (headerTable.contains(headerNameLowerCase) == false) {
//      headerTable += (headerNameLowerCase -> sipHeader)
//      headers :+ sipHeader
//    } else {
//      if (sipHeader.isInstanceOf[SIPHeaderList]) {
//        val hdrList = headerTable.get(headerNameLowerCase).map(_.asInstanceOf[SIPHeaderList])
//        if (hdrList.isDefined)
//          hdrList.get.concatenate(sipHeader.asInstanceOf[SIPHeaderList], top)
//        else
//          headerTable.put(headerNameLowerCase, h);
//      } else {
//        headerTable.put(headerNameLowerCase, h);
//      }
//    }
//
//  }

  def addHeader(header: Header) = null
}
