import sbt.Keys.scalacOptions

val commonSettings = Seq(
  scalaVersion := "2.12.1"
)

val catSettings = Seq(
  scalaVersion := "2.12.8"
)

val circeVersion = "0.11.1"

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )

lazy val catExercises = (project in file("cats-exercises"))
  .settings(catSettings)
  .settings(
    name := "cats-exercises",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M1"
  )

lazy val experiments = (project in file("experiments"))
  .settings(catSettings)
  .settings(
    name := "experiments",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion) ++
      Seq("org.typelevel" %% "cats-core" % "2.0.0-M1") ++
      Seq("com.github.pathikrit" %% "better-files" % "3.8.0" )
    )
  







