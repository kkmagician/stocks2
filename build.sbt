name := "stocks2"

version := "1.1"
scalaVersion := "2.13.2"

val circeVersion = "0.13.0"
val http4sVersion = "0.21.0"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl",
  "org.http4s" %% "http4s-blaze-client",
  "org.http4s" %% "http4s-circe"
).map(_ % http4sVersion) ++ Seq(
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-optics",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion) ++ Seq(
  "org.slf4j" % "slf4j-api" % "1.7.30",
  "org.slf4j" % "slf4j-simple" % "1.7.30",
  "org.scalactic" %% "scalactic" % "3.1.2",
  "org.scalatest" %% "scalatest" % "3.1.2" % "test"
)

enablePlugins(DockerPlugin)
enablePlugins(JavaAppPackaging)

packageName in Docker := "stocks2"
version in Docker := version.value
dockerBaseImage := "openjdk:15-slim"
dockerRepository := Some("registry.gitlab.com")
dockerUsername := Some("awfrke")
dockerUpdateLatest := true
