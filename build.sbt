
// For Settings/Task reference, see http://www.scala-sbt.org/release/sxr/sbt/Keys.scala.html

lazy val commonSettings = Seq(
  organization := "org.sgp4s_11",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7"),
  libraryDependencies ++= Seq(
    "org.spire-math" %%% "spire" % "0.11.0",
    "org.scalactic" %%% "scalactic" % "3.0.0-M15" % "test",
    "org.scalatest" %%% "scalatest" % "3.0.0-M15" % "test"
  ),
// Compiler settings. Use scalac -X for other options and their description.
// See Here for more info http://www.scala-lang.org/files/archive/nightly/docs/manual/html/scalac.html 
  scalacOptions ++= Seq(
    "-deprecation", "-unchecked", "-Xlint", "-encoding", "UTF-8","-language:implicitConversions","-language:higherKinds", "-feature"
  ),
  licenses += ("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("http://github.com/pleira/sgp4s_11")))


lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false)

lazy val root = project.in(file("."))
  .aggregate(
    coreJVM, testsJVM
  )
  .settings(name := "root")
  .settings(commonSettings: _*)
  .settings(noPublish: _*)

lazy val core = crossProject.crossType(CrossType.Pure).in(file("core"))
  .settings(name := "sgp4s_11")
  .settings(commonSettings: _*)

lazy val tests = crossProject.crossType(CrossType.Pure).in(file("tests"))
  .settings(name := "sgp4s_11-tests")
  .settings(commonSettings: _*)
  .settings(noPublish: _*)
  .dependsOn(core)

lazy val reports = project.in(file("reports"))
  .settings(name := "reports")
  .settings(commonSettings:_*)
  .dependsOn(coreJVM)
  .settings(libraryDependencies +=
    "com.github.pathikrit" %% "better-files" % "2.14.0") 
  .settings(noPublish: _*)

lazy val thymeBenchmarks = project.in(file("thymeBenchmarks"))
  .settings(name := "thymeBenchmarks")
  .settings(commonSettings:_*)
  .dependsOn(coreJVM)
  .settings(libraryDependencies +=
    "ichi.bench" % "thyme" % "0.1.1" from "https://github.com/Ichoran/thyme/raw/9ff531411e10c698855ade2e5bde77791dd0869a/Thyme.jar")
  .settings(noPublish: _*)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js

lazy val testsJVM = tests.jvm

lazy val testsJS = tests.js

