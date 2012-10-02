package org.sisioh.sip.message

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

import address.URI

trait Request extends Message {
  val method: Option[String]
  val requestURI: Option[URI]
}

object Request {
  val ACK = "ACK"
  val BYE = "BYE"
  val CANCEL = "CANCEL"
  val INVITE = "INVITE"
  val OPTIONS = "OPTIONS"
  val REGISTER = "REGISTER"
  val NOTIFY = "NOTIFY"
  val SUBSCRIBE = "SUBSCRIBE"
  val MESSAGE = "MESSAGE"
  val REFER = "REFER"
  val INFO = "INFO"
  val PRACK = "PRACK"
  val UPDATE = "UPDATE"
  val PUBLISH = "PUBLISH"
}
