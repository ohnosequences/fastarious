name          := "fastarious"
organization  := "ohnosequences"
description   := "FASTQ and FASTA APIs"
bucketSuffix  := "era7.com"

crossScalaVersions := Seq("2.11.11", "2.12.3")
scalaVersion := crossScalaVersions.value.max

libraryDependencies ++= Seq(
  "ohnosequences" %% "cosas" % "0.9.0"
)

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")
// disables parallel exec
parallelExecution in Test := false
