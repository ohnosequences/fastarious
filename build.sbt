name          := "fastarious"
organization  := "ohnosequences"
description   := "FASTQ and FASTA APIs"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences"   %% "cosas" % "0.8.0",
  "org.spire-math"  %% "spire" % "0.13.0"
)

// NOTE should be reestablished
wartremoverErrors in (Test,    compile) := Seq()
wartremoverErrors in (Compile, compile) := Seq()

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")
// disables parallel exec
parallelExecution in Test := false
