import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

def generateIndexTask(index: String, suffix: String) = Def.task {
  val source = baseDirectory.value / "index.html"
  val target = (Compile / crossTarget).value / index
  val log = streams.value.log
  IO.writeLines(target,
    IO.readLines(source).map {
      line => line.replace("{{target-js}}", s"formulafx-$suffix.js")
    }
  )

  log.info(s"Generate $index with suffix: $suffix")
}

lazy val commonSettings = Seq(
  scalaVersion := "2.13.8",
  version := "0.2.0-beta",
  libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.0" % "test"
)
lazy val root = project.in(file(".")).
  aggregate(pJVM, pJS).
  settings(
    name := "FormulaFX",
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
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
    assemblyJarName := "FormulaFX.jar"
  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.2.0",
    (Compile / fastOptJS) := (Compile / fastOptJS).dependsOn(generateIndexTask("index-fast.html","fastOpt")).value,
    (Compile / fullOptJS) := (Compile / fullOptJS).dependsOn(generateIndexTask("index.html","opt")).value
  )

lazy val pJVM = projs.jvm
lazy val pJS = projs.js

