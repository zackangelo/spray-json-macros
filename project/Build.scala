import sbt._ 
import Keys._ 

object SprayJsonMacrosBuild extends Build { 
	lazy val commonSettings = Seq(
		organization := "zangelo", 
		version := "0.0.1", 
		scalaVersion := "2.11.6",
		resolvers ++= Seq(
			"Spray Repo" 			at "http://repo.spray.io",
			"Spray Nightlies Repo" 	at "http://nightlies.spray.io"
		),
		libraryDependencies ++= Seq(
			"io.spray" %% "spray-json" % "1.3.1",
			"org.scala-lang" % "scala-compiler" % scalaVersion.value,
			"org.scala-lang" % "scala-reflect" % scalaVersion.value,
			"org.specs2" %% "specs2-core" % "2.4.16" % "test",
			"org.specs2" %% "specs2-scalacheck" % "2.4.16" % "test",
			"org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
		),
		scalacOptions ++= Seq("-deprecation")
	)

	lazy val root = Project(id = "spray-json-macros", base = file(".")).settings(commonSettings: _*)
}