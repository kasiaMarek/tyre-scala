lazy val basicSettings = Seq(
  organization := "net.marek",
  organizationName := "Marek",
  organizationHomepage := Some(url("http://marek.net")),
  startYear := Some(2023),
  name := "tyre-scala",
  description := "Typed regex parser",
  scalaVersion := "3.3.0"
)

addCommandAlias("check", "; scalafmtCheck ; scalafix --check")

lazy val root = (project in file("."))
  .settings(basicSettings: _*)
  .settings(
    licenses += ("Apache-2.0", new URI("https://www.apache.org/licenses/LICENSE-2.0.txt").toURL),
    versionScheme := Some("semver-spec"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" % "scala-parser-combinators_3" % "2.3.0",
      "org.scalatest" %% "scalatest" % "3.2.16" % Test
    ),
    Test / fork := true,
    javaOptions += "-Dfile.encoding=UTF-8",
    scalacOptions ++= scalacSettings,
    Compile / console / scalacOptions --= Seq("-Xfatal-warnings"),
    semanticdbEnabled := true
  )

lazy val scalacSettings = Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-explain", // Explain errors in more detail.
  "-explain-types", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:higherKinds,implicitConversions", // Enable language features.
  "-new-syntax",  // Require Scala 3 syntax
  "-pagewidth:120", // Set output page width.
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Wvalue-discard", // Warn about unused expression results.
  "-Wunused:all",  // Warn about unused code
  "-Wconf:cat=deprecation:w,any:e", // Fail the compilation if there are any warnings except deprecation.
  "-Xtarget:17", // Set target JVM version.
  "-Xverify-signatures" // Verify generic signatures in generated bytecode.
)
