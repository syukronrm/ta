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
