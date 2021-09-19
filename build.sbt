// scala-demo

ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "wafna"

ThisBuild / run / fork := true
ThisBuild / test / run / fork := true

ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")

val typesafeVersion = "1.4.1"
val logbackVersion = "1.2.5"
val slf4jVersion = "1.7.32"
val akkaVersion = "2.6.16"
val akkaHttpVersion = "10.2.6"
val scalaTestVersion = "3.2.9"
val dockerTestKitVersion = "0.9.9"

val typesafeConfig = "com.typesafe" % "config" % typesafeVersion

val slf4jApi = "org.slf4j" % "slf4j-api" % slf4jVersion
val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
val log4j = "log4j" % "log4j" % "1.2.17"

val akkaActorTyped = "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion
val akkaStream = "com.typesafe.akka" %% "akka-stream" % akkaVersion
val akkaHttp = "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
val akkaSprayJson = "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion
val akkaActorTestKitTyped = "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion

val javaxActivation = "javax.activation" % "activation" % "1.1.1"

val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
val dockerTestKit = "com.whisk" %% "docker-testkit-scalatest" % dockerTestKitVersion
val dockerSpotifyTestKit = "com.whisk" %% "docker-testkit-impl-spotify" % dockerTestKitVersion

Compile / scalacOptions += "-Xfatal-warnings"
Test / run / fork := true

val AllTest = "it,test"

libraryDependencies ++= Seq(
  slf4jApi,
  logbackClassic,
  typesafeConfig,
  javaxActivation,
  scalaTest % AllTest,
  dockerSpotifyTestKit % IntegrationTest,
  akkaActorTestKitTyped % IntegrationTest,
  dockerTestKit % IntegrationTest
)
