// scala-demo

ThisBuild / scalaVersion := "3.1.1"
ThisBuild / organization := "wafna"

ThisBuild / run / fork := true
ThisBuild / test / run / fork := true

ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")

Compile / scalacOptions += "-Xfatal-warnings"
Test / run / fork := true

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  "com.typesafe" % "config" % "1.4.1",
  "org.slf4j" % "slf4j-api" % "1.7.35",
  "ch.qos.logback" % "logback-classic" % "1.2.10",
  "log4j" % "log4j" % "1.2.17",
  "org.typelevel" %% "cats-core" % "2.7.0",
  "io.dropwizard.metrics" % "metrics-core" % "4.2.7",
  "nl.grons" %% "metrics4-scala" % "4.2.8",
  "nl.grons" %% "metrics4-akka_a26" % "4.2.8",
  "nl.grons" %% "metrics4-scala-hdr" % "4.2.8"
)
