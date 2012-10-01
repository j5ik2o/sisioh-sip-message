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

import org.sisioh.sip.message.header.{Header, ContentDispositionHeader, ContentTypeHeader}
import org.sisioh.sip.core.Separators

case class DefaultConent
(content: Any,
 contentTypeHeader: Option[ContentTypeHeader],
 contentDispositionHeader: Option[ContentDispositionHeader]
  ) extends Conent {

  private val _extensionHeaders: List[Header] = List()

  def extensionHeaders: Iterator[Header] = _extensionHeaders.iterator

  override def toString = {
    val sb = new StringBuilder
    if (contentTypeHeader.isDefined) {
      sb.append(contentTypeHeader.get.toString)
    }
    if (contentDispositionHeader.isDefined) {
      sb.append(contentDispositionHeader.get.toString)
    }
    _extensionHeaders.foreach {
      e =>
        sb.append(e)
    }
    sb.append(Separators.NEWLINE)
    sb.append(content.toString)
    sb.result()
  }
}
