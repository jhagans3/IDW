name := "IDW"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.processing" % "core" % "3.2.3"
)

assemblyJarName in assembly := "IDW.jar"
mainClass in assembly := Some("ProcessingApp")
