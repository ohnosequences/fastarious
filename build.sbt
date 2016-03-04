Nice.scalaProject

name          := "fastarious"
organization  := "ohnosequences"
description   := "fastarious project"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences"         %% "cosas"        % "0.8.0",
  "com.github.pathikrit"  %% "better-files" % "2.13.0",

  "org.scalatest" %% "scalatest" % "2.2.5" % Test
)

wartremoverExcluded ++= Seq(
  baseDirectory.value/"src"/"main"/"scala"/"fasta.scala"
)

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")
// disables parallel exec
parallelExecution in Test := false
