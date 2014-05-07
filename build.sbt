import sbt._

name := "kontopro"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.6",
  "ch.qos.logback" % "logback-core" % "1.1.1",
  "org.scalatest" %% "scalatest" % "2.1.0-RC3",
  "org.scalaz" %% "scalaz-core" % "7.1.0-M5",
  "junit" % "junit" % "4.11",
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-money" % "0.9.1",
  "org.joda" % "joda-convert" % "1.6")
