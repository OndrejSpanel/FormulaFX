name := "FormulaFX"

version := "0.0.3-alpha"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.60-R9"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies +=   "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// Fork a new JVM for 'run' and 'test:run', to avoid JavaFX double initialization problems
fork := true

enablePlugins(JavaAppPackaging)

// general package information (can be scoped to Windows)
maintainer := "Ondrej Spanel <OndrejSpanel@users.noreply.github.com>"
packageSummary := "FormulaFX windows installer"
packageDescription := """Install FormulaFX calculator on Windows."""

// wix build information
wixProductId := "f95a392c-fdf0-46b8-a55a-602f9c7927a6"
wixProductUpgradeId := "44a837b8-c748-4040-a71d-415c4b41c2b0"