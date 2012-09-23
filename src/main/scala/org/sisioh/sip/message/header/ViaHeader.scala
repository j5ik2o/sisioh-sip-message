package org.sisioh.sip.message.header

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

trait ViaHeader {
  val host: String
  val port: Option[Int]
  val transport: String
  val protocol: String
  val ttl: Option[Int]
  val maddr: String
  val received: Option[String]
  val branch: Option[String]
  val rPort: Option[Int]
}
object ViaHeader {
  val NAME = "Via"
}
