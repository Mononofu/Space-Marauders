name := "Space Marauders"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

seq(lwjglSettings: _*)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.1.4"

  libraryDependencies +=
    "com.typesafe.akka" %% "akka-testkit" % "2.1.4"
