name := "FormulaFX"

version := "0.0.1-alpha"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.60-R9"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies +=   "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// Fork a new JVM for 'run' and 'test:run', to avoid JavaFX double initialization problems
fork := true
