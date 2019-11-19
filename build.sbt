import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

scalaVersion in ThisBuild := "2.11.8"

def generateIndexTask(index: String, suffix: String) = Def.task {
  val source = baseDirectory.value / "index.html"
  val target = (crossTarget in Compile).value / index
  val log = streams.value.log
  IO.writeLines(target,
    IO.readLines(source).map {
      line => line.replace("{{target-js}}", s"formulafx-$suffix.js")
    }
  )

  log.info(s"Generate $index with suffix: $suffix")
}

lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  version := "0.1.1-alpha",
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)
lazy val root = project.in(file(".")).
  aggregate(pJVM, pJS).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val projs = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("."))
  .settings(
    commonSettings,
    name := "FormulaFX",
    maxErrors := 1,
    scalacOptions := Seq("-unchecked", "-deprecation")
  )
  .jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"
  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    (fastOptJS in Compile) := (fastOptJS in Compile).dependsOn(generateIndexTask("index-fast.html","fastOpt")).value,
    (fullOptJS in Compile) := (fullOptJS in Compile).dependsOn(generateIndexTask("index.html","opt")).value
  )

lazy val pJVM = projs.jvm
lazy val pJS = projs.js

