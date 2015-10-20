Nice.scalaProject

name          := "fastarious"
organization  := "ohnosequences"
description   := "fastarious project"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences" %% "cosas" % "0.7.1",
  "org.scalatest" %% "scalatest" % "2.2.5" % Test
)
