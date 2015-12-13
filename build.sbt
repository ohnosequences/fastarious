Nice.scalaProject

name          := "fastarious"
organization  := "ohnosequences"
description   := "fastarious project"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences" %% "cosas" % "0.8.0"
  "org.scalatest" %% "scalatest" % "2.2.5" % Test
)
