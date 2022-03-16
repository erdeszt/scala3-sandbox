val dottyVersion = "3.0.1"

val scalatestVersion = "3.2.9"
val fs2Version       = "3.0.6"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-sandbox",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
      "org.typelevel" %% "cats-effect" % "3.2.1",
      "co.fs2" %% "fs2-core" % fs2Version,
      "co.fs2" %% "fs2-io" % fs2Version,
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
      "org.scalatest" %% "scalatest-funspec" % scalatestVersion % Test,
      "org.scalatest" %% "scalatest-freespec" % scalatestVersion % Test
    ),
    scalacOptions ++= Seq(
      "-Ykind-projector"
    )
  )
