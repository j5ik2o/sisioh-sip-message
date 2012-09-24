package org.sisioh.sip.message.impl

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
  val remoteAddress: InetAddress
  val remotePort: Int
  val localAddress: InetAddress
  val localPort: Int

}
