name := "physical-scalar"
version := "4.4"
scalaVersion := "2.13.1"
organization := "gov.nasa.arc"

crossPaths := false

scalaSource in Compile := baseDirectory.value / "src" / "main"
scalaSource in Test    := baseDirectory.value / "src" / "test"

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

scalacOptions += "-deprecation"
scalacOptions += "-Xcheckinit"
scalacOptions += "-feature"
scalacOptions += "-Ywarn-value-discard"
scalacOptions += "-Ywarn-dead-code"
//scalacOptions += "-Xdisable-assertions"
//scalacOptions += "-Xfatal-warnings"
//scalacOptions += "-Xlint"
//scalacOptions += "-Ywarn-unused"
//scalacOptions += "-Ywarn-unused:imports"
