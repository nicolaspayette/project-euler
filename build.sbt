name := "project-euler"

version := "1.0"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test",
  "org.scalactic" % "scalactic_2.10" % "2.2.1"
)