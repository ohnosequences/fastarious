name          := "fastarious"
organization  := "ohnosequences"
description   := "fastarious project"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences" %% "cosas" % "0.8.0"
)

// NOTE should be reestablished
wartremoverErrors in (Test,    compile) := Seq()
wartremoverErrors in (Compile, compile) := Seq()

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")
// disables parallel exec
parallelExecution in Test := false
