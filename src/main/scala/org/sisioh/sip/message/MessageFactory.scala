package org.sisioh.sip.message

import address.URI
import header._

trait MessageFactory {

  def createRequest[A]
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: A): Request[A]

  def createRequest
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Array[Byte]): Request[Array[Byte]]

  def createRequest[T]
  (requestURI: URI,
   method: String,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader): Request[T]


  def createResponse[A]
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: A): Response[A]

  def createResponse
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader,
   contentType: ContentTypeHeader,
   content: Array[Byte]): Response[Array[Byte]]

  def createResponse[A]
  (statusCode: StatusCode.Value,
   callId: CallIdHeader,
   cseq: CSeqHeader,
   from: FromHeader,
   to: ToHeader): Response[A]

  def createResponse[A, B]
  (statusCode: StatusCode.Value,
   request: Request[A],
   contentType: ContentTypeHeader,
   content: B): Response[B]

  def createResponse[A]
  (statusCode: StatusCode.Value,
   request: Request[A],
   contentType: ContentTypeHeader,
   content: Array[Byte]): Response[Array[Byte]]

  def createResponse[A, B]
  (statusCode: StatusCode.Value,
   request: Request[A]): Response[B]

  def createRequest[A](requestString: String): Request[A]

  def createResponse[A](responseString: String): Response[A]


}
