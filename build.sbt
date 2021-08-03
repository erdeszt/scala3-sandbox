val dottyVersion = "3.0.1"

val scalatestVersion = "3.2.9"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-sandbox",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.2.1",
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
      "org.scalatest" %% "scalatest-funspec" % scalatestVersion % Test,
      "org.scalatest" %% "scalatest-freespec" % scalatestVersion % Test
    ),
    scalacOptions ++= Seq("-Ykind-projector", "-Wall")
  )
