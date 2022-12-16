ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "plataformaBusqueda"
  )

scalacOptions += "-deprecation"

// ScalaTest + JUnit dependencies
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test"
libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.11.0" % "test"
