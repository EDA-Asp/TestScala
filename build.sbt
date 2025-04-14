ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.4"

lazy val root = (project in file("."))
  .settings(
    name := "HelloWrld"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"


scalacOptions ++= Seq("-Wsafe-init")