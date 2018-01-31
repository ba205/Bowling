import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Bowling",
    libraryDependencies ++= Seq(scalaTest % Test, 
      "org.scalaz" %% "scalaz-core" % "7.2.18")
  )
