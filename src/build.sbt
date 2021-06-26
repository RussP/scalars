name := "physical-scalar"
version := "5.0"
scalaVersion := "3.0.0"
organization := "gov.nasa.arc"

crossPaths := false

Compile / scalaSource := baseDirectory.value/"src"/"main"
Test / scalaSource    := baseDirectory.value/"src"/"test"

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"

scalacOptions += "-deprecation"
//scalacOptions += "-Xcheckinit"
scalacOptions += "-feature"
//scalacOptions += "-Ywarn-value-discard"
//scalacOptions += "-Ywarn-dead-code"
//scalacOptions += "-Xdisable-assertions"
//scalacOptions += "-Xfatal-warnings"
//scalacOptions += "-Xlint"
//scalacOptions += "-Ywarn-unused"
//scalacOptions += "-Ywarn-unused:imports"
