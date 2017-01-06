name := "SpiralSTarget"

version := "0.1"

organization := "ETH"

scalaVersion := "2.12.1"

scalaSource in Compile <<= baseDirectory(_ / "src/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-library" % _ % "compile")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "compile")




//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.3" % "test"

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
val contVersion = "1.0.2"

autoCompilerPlugins := true





// code coverage

