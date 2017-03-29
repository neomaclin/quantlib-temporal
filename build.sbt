organization := "org.quantlib"

name := "quantlib-temporal"

version := "1.0"

scalaVersion := "2.12.0"

lazy val catz = Seq("org.typelevel" %% "cats" % "0.9.0")
lazy val scalaTest = Seq("org.scalatest" %% "scalatest" % "3.0.0" % "test")

libraryDependencies ++= scalaTest ++ catz
