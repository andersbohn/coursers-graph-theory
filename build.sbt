lazy val root = (project in file(".")).
  settings(
    organization := "dk.andersbohn",
    name := "coursera-graph-theory",
    scalaVersion := "2.12.4",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scala-graph" %% "graph-core" % "1.12.3",
      "org.scala-graph" %% "graph-dot" % "1.12.1",
      "org.scalatest" %% "scalatest" % "3.0.1"
    )
  )
