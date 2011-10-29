name := "pom-util"

version := "1.0-SNAPSHOT"

organization := "com.samskivert"

scalaVersion := "2.9.1"

crossPaths := false

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.7" % "test->default"
)
