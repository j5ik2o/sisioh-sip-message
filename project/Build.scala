import sbt._
import Keys._

object MyBuild extends Build {

  lazy val root = Project(
    id = "sisioh-sip-message",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "sisioh-sip-message",
      scalaVersion := "2.9.1",
      resolvers += ScalaToolsSnapshots,
      scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
      javacOptions ++= Seq("-encoding", "UTF-8", "-deprecation"),
      resolvers ++= Seq(
        "Twitter Repository" at "http://maven.twttr.com/",
        "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
        "Sisioh Maven Relase Repository" at "http://maven.sisioh.org/release/",
        "Sisioh Maven Snapshot Repository" at "http://maven.sisioh.org/snapshot/"
      ),
      libraryDependencies ++= Seq(
        "junit" % "junit" % "4.8.1" % "test",
        "org.hamcrest" % "hamcrest-all" % "1.1" % "test",
        "org.mockito" % "mockito-all" % "1.8.5" % "test",
        "org.specs2" %% "specs2" % "1.11" % "test",
        "org.slf4j" % "slf4j-api" % "1.6.1",
        "org.slf4j" % "jcl-over-slf4j" % "1.6.1",
        "ch.qos.logback" % "logback-classic" % "0.9.28",
        "org.clapper" %% "grizzled-slf4j" % "0.6.6",
        "com.twitter" % "finagle-core" % "5.3.8",
        "com.twitter" % "finagle-http" % "5.3.8",
        "com.twitter" % "finagle-stream" % "5.3.8",
        "com.twitter" % "finagle-redis" % "5.3.8",
        "commons-httpclient" % "commons-httpclient" % "3.1",
        "org.mortbay.jetty" % "jetty-client" % "6.1.26",
        "net.liftweb" % "lift-json_2.9.1" % "2.4",
        "net.liftweb" % "lift-json-ext_2.9.1" % "2.4",
        "org.sisioh" %% "scala-dddbase-core" % "0.0.1",
        "org.sisioh" %% "scala-dddbase-spec" % "0.0.1",
        "org.sisioh" %% "baseunits-scala" % "0.0.1",
        "org.scalaz" %% "scalaz-core" % "6.0.3",
        "redis.clients" % "jedis" % "2.0.0"
      )
    )
  )
}
