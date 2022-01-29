// scala-demo

ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "wafna"

ThisBuild / run / fork := true
ThisBuild / test / run / fork := true

ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")

val typesafeVersion = "1.4.1"
val logbackVersion = "1.2.10"
val slf4jVersion = "1.7.33"
val akkaVersion = "2.6.16"
val akkaHttpVersion = "10.2.6"
val scalaTestVersion = "3.2.9"
val dockerTestKitVersion = "0.9.9"

val typesafeConfig = "com.typesafe" % "config" % typesafeVersion

val slf4jApi = "org.slf4j" % "slf4j-api" % slf4jVersion
val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
val log4j = "log4j" % "log4j" % "1.2.17"

val javaxActivation = "javax.activation" % "activation" % "1.1.1"

val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion

val catsCore = "org.typelevel" %% "cats-core" % "2.7.0"

Compile / scalacOptions += "-Xfatal-warnings"
Test / run / fork := true

libraryDependencies ++= Seq(slf4jApi, logbackClassic, typesafeConfig, javaxActivation, scalaTest % Test)
