import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "plus.meow"
ThisBuild / organizationName := "CircuitCoder"

lazy val root = (project in file("."))
  .settings(
    name := "MeowRust",
    libraryDependencies += scalaTest % Test
  )
