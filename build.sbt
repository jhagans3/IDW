name := "M8ForGIS"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.processing" % "core" % "3.2.3"
)

assemblyJarName in assembly := "Final_Hagans_Sagang.jar"
mainClass in assembly := Some("ProcessingApp")