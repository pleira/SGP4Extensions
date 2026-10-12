lazy val commonSettings = Seq(
  organization := "org.sgp4s",
  scalaVersion := "2.13.18",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "spire" % "0.18.0",
    "org.scalactic" %% "scalactic" % "3.2.20",
    "org.scalatest" %% "scalatest" % "3.2.20" % Test
  ),
  scalacOptions ++= Seq(
    "-deprecation", "-unchecked", "-Xlint", "-encoding", "UTF-8",
    "-language:implicitConversions", "-language:higherKinds", "-feature"
  ),
  licenses += ("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("http://github.com/pleira/SGP4Extensions"))
)

lazy val commonJvmSettings = Seq(
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false)

lazy val root = (project in file("."))
  .aggregate(core, tests, reports, benchmarks)
  .settings(name := "root")
  .settings(commonSettings: _*)
  .settings(noPublish: _*)

lazy val core = (project in file("core"))
  .settings(name := "sgp4ext-core")
  .settings(commonSettings: _*)

lazy val tests = (project in file("tests"))
  .settings(name := "sgp4ext-tests")
  .settings(commonSettings: _*)
  .settings(commonJvmSettings: _*)
  .dependsOn(core % "compile->compile;test->test")
  .settings(noPublish: _*)

lazy val reports = (project in file("reports"))
  .settings(name := "sgp4ext-reports")
  .settings(commonSettings: _*)
  .settings(commonJvmSettings: _*)
  .dependsOn(core, tests % "compile->compile;test->test")
  .settings(libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.2")
  .settings(noPublish: _*)

// JMH micro-benchmarks for the propagation hot path; replaced the old
// thymeBenchmarks module, whose Thyme dependency is no longer available.
lazy val benchmarks = (project in file("benchmarks"))
  .settings(name := "sgp4ext-benchmarks")
  .settings(commonSettings: _*)
  .dependsOn(core, tests)
  .enablePlugins(JmhPlugin)
  .settings(noPublish: _*)
