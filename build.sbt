name := "SpiralSTarget"

version := "0.1"

organization := "ETH"

scalaVersion := "2.11.7"

scalaSource in Compile <<= baseDirectory(_ / "src/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-library" % _ % "compile")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "compile")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" //% "test" // cross CrossVersion.full"

libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.6"

libraryDependencies += "com.github.scala-blitz" %% "scala-blitz" % "1.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6" % "test"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.0" //% "test"
)

//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.3" % "test"

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
val contVersion = "1.0.2"

autoCompilerPlugins := true

libraryDependencies ++= Seq(
  "org.scala-lang.plugins" %% "scala-continuations-library" % contVersion % "compile"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
  deps :+ compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin" % contVersion cross CrossVersion.full)
}

scalacOptions += "-P:continuations:enable"

val paradiseVersion = "2.0.1"

libraryDependencies ++= (
  if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
  else Nil
  )

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "compile")

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

// code coverage

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"