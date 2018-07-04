name := "init_test"

version := "0.1"

scalaVersion := "2.12.5"

resolvers += "bintray/meetup" at "http://dl.bintray.com/meetup/maven"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "org.scala-graph" %% "graph-core" % "1.12.3"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

// https://github.com/non/archery
libraryDependencies += "org.spire-math" %% "archery" % "0.6.0"
libraryDependencies += "com.github.davidmoten" % "rtree" % "0.8.5"
libraryDependencies += "uk.com.robust-it" % "cloning" % "1.9.10"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.12",
  "com.typesafe.akka" %% "akka-http"   % "10.1.3",
  "com.typesafe.akka" %% "akka-stream" % "2.5.12",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.12" % Test
)

val circeVersion = "0.9.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.3"

enablePlugins(JmhPlugin)
