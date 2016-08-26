import org.scalajs.sbtplugin.cross.CrossType

scalaVersion in ThisBuild := "2.11.8"

// settings here used by IDE for highlighting
libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.60-R9"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

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

lazy val root = project.in(file(".")).
  aggregate(pJVM, pJS).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val myCrossType = new CrossType {
  // similar to CrossType.Full, but with a different sharedSrcDir
  def projectDir(crossBase: File, projectType: String): File = crossBase / projectType

  def sharedSrcDir(projectBase: File, conf: String): Option[File] = Some(projectBase.getParentFile / "src" / conf / "scala")
}

lazy val projs = crossProject.crossType(
  myCrossType
).
  in(file(".")).
  settings(
    name := "FormulaFX",
    version := "0.0.8-alpha",
    maxErrors := 1
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.60-R9",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    // Fork a new JVM for 'run' and 'test:run', to avoid JavaFX double initialization problems
    fork := true
  ).
  jsSettings(
    libraryDependencies += "org.scala-js" %%% "scala-parser-combinators" % "1.0.2",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0-M15" % "test",
    (fastOptJS in Compile) <<= (fastOptJS in Compile).dependsOn(generateIndexTask("index-fast.html","fastOpt")),
    (fullOptJS in Compile) <<= (fullOptJS in Compile).dependsOn(generateIndexTask("index.html","opt"))

  )

lazy val pJVM = projs.jvm
lazy val pJS = projs.js

