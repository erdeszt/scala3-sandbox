val dottyVersion = "3.0.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-sandbox",
    version := "0.1.0",
    scalaVersion := dottyVersion,
  )
