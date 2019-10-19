import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "MeowPlus"
ThisBuild / organizationName := "CircuitCoder"
ThisBuild / mainClass        := Some("plus.meow.MeowRust.Main")

lazy val root = (project in file("."))
  .settings(
    name := "MeowRust",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += gllCombinator
  )
