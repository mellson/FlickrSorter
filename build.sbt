name := """FlickrSorter"""

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.9.1" % "test",
    "uk.co.bigbeeconsultants" %% "bee-client" % "0.21.+",
    "org.slf4j" % "slf4j-api" % "1.7.+",
    "ch.qos.logback" % "logback-core"    % "1.0.+",
    "ch.qos.logback" % "logback-classic" % "1.0.+"
)

resolvers += "Big Bee Consultants" at "http://repo.bigbeeconsultants.co.uk/repo"