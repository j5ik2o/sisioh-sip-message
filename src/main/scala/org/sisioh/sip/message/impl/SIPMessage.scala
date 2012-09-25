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

}
