package org.sisioh.sip.message.header.impl

//import org.sisioh.sip.message.header.{SIPHeaderNames, Header}
//import org.sisioh.sip.core.Separators
//import collection.immutable.List
//import collection.generic.{GenericTraversableTemplate, CanBuildFrom, SeqFactory, GenericCompanion}
//import collection.{mutable, immutable, LinearSeqOptimized}
//import collection.mutable.{Builder, ListBuffer}
//import org.sisioh.baseunits.scala.intervals.IntervalSeqBuilder
//
//class SIPHeaderListBuilder[HDR <: SIPHeader]
//(clazz: Class[HDR],
// headerName: String,
// prettyEncode: Boolean = false)
//  extends Builder[HDR, SIPHeaderList[HDR]] {
//
//  val buffer = ListBuffer[HDR]()
//
//  def +=(elem: HDR) = {
//    buffer += elem
//    this
//  }
//
//  def clear() {
//    buffer.clear()
//  }
//
//  def result() = {
//    new SIPHeaderList(clazz, headerName, prettyEncode)
//  }
//
//}
//
//object SIPHeaderList {
//
//  type From[T] = Seq[T]
//  type Elem[T] = T
//  type To[T] = SIPHeaderList[T]
//
//  implicit def canBuildFrom[T <: SIPHeader]: CanBuildFrom[From[T], Elem[T], To[T]] =
//    new CanBuildFrom[From[T], Elem[T], To[T]] {
//
//      def apply(from: From[T]) = {
//        from match {
//          case intervalSeq: SIPHeaderList[T] => new SIPHeaderListBuilder[T]()
//          case _ => throw new Error
//        }
//      }
//
//      def apply() = new SIPHeaderListBuilder[T]()
//
//    }
//
//  def newBuilder[A <: SIPHeader]
//  (clazz: Class[A],
//   headerName: String,
//   prettyEncode: Boolean = false): Builder[A, SIPHeaderList[A]] =
//    new SIPHeaderListBuilder(clazz, headerName, prettyEncode)
//
//}
//
//class SIPHeaderList[HDR <: SIPHeader]
//(val clazz: Class[HDR],
// val headerName: String,
// prettyEncode: Boolean = false)
//  extends SIPHeader with Header
//  with immutable.LinearSeq[HDR] {
//
//  //  with scala.Product
//  //  with GenericTraversableTemplate[HDR, SIPHeaderList]
//  //  with LinearSeqOptimized[HDR, SIPHeaderList[HDR]] {
//
//  override protected[this] def newBuilder = SIPHeaderList.newBuilder()
//
//  val name = headerName
//  private val headers = List.empty[HDR]
//
//  def encodeBody(builder: StringBuilder) = headers match {
//    case Nil =>
//      builder.append(headerName).append(':').append(Separators.NEWLINE)
//    case _ =>
//      if (headerName == SIPHeaderNames.WWW_AUTHENTICATE |
//        headerName == SIPHeaderNames.PROXY_AUTHENTICATE |
//        headerName == SIPHeaderNames.AUTHORIZATION |
//        headerName == SIPHeaderNames.PROXY_AUTHORIZATION |
//        prettyEncode && headerName == SIPHeaderNames.VIA |
//        clazz == classOf[ExtensionHeaderList] |
//        headerName == SIPHeaderNames.ROUTE) {
//        headers.foreach {
//          header =>
//            header.encode(builder)
//        }
//        builder
//      } else {
//        builder.append(headerName).append(Separators.COLON).append(Separators.SP)
//        encodeBody(builder)
//        builder.append(Separators.NEWLINE)
//      }
//  }
//
//  def productArity = 0
//
//}
