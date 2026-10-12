// No build plugins required.
// sbt-scoverage (code coverage) and scalastyle (style checker) were dropped
// during the sbt 1.x / Scala 2.13 migration; re-add them if needed.

// JMH micro-benchmarks for the benchmarks module
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

// Code formatting
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")
