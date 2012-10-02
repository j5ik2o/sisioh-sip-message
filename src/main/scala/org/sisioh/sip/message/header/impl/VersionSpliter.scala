package org.sisioh.sip.message.header.impl

trait VersionSpliter {

  protected def versionSplis(version: Option[String], index: Int) = {
    version.flatMap {
      e =>
        val versions = e.split('/').toList match {
          case List(l) => Some(l)
          case List(_, l) => Some(l)
          case _ => None
        }
        versions.flatMap {
          v =>
            v.toArray.toList match {
              case List(major, '.', _) if (index == 0) =>
                Some(major.toString)
              case List(_, '.', minar) if (index == 1) =>
                Some(minar.toString)
              case _ => None
            }
        }
    }
  }

}
