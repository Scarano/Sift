name := "Sift"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1"

//libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

//libraryDependencies += "com.github.guillaumedd" %% "gstlib" % "0.1.2"

// https://mvnrepository.com/artifact/net.seninp/jmotif-gi
libraryDependencies += "net.seninp" % "jmotif-gi" % "1.0.1"

// https://mvnrepository.com/artifact/com.github.haifengl/smile-core
libraryDependencies += "com.github.haifengl" % "smile-core" % "1.5.3"
libraryDependencies += "com.github.haifengl" %% "smile-scala" % "1.5.3"
libraryDependencies += "com.github.haifengl" % "smile-netlib" % "1.5.3"

// https://mvnrepository.com/artifact/org.scalanlp/breeze
libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "1.0"


mainClass in (Compile, run) := Some("TreeSandbox")


// Copy "resources" to target directory
resourceDirectory in Compile := file("resources")

fork in run := true


////////////////
// From http://ammonite.io/#Ammonite-REPL
libraryDependencies += {
  "com.lihaoyi" % "ammonite" % "1.8.2" cross CrossVersion.full
}

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

// Optional, required for the `source` command to work
//(fullClasspath in Test) ++= {
//  (updateClassifiers in Test).value
//    .configurations
//    .find(_.configuration == Test.name)
//    .get
//    .modules
//    .flatMap(_.artifacts)
//    .collect{case (a, f) if a.classifier == Some("sources") => f}
//}

// Prevent sbt from forking when running Ammonite with `sbt test:run`
Test / run / fork := false

//////////////
