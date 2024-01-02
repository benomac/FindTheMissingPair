ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
  "org.scalameta" %% "munit" % "0.7.29" % Test,
  "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4",
  "org.typelevel" %% "munit-cats-effect-3" % "1.0.7"
)

lazy val root = (project in file("."))
  .settings(
    name := "FindTheMissingPairCardTrick"
  )

