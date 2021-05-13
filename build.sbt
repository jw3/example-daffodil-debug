name := "daffodil-debugger"

git.useGitDescribe := true

javacOptions ++= Seq("-source", "11")
scalacOptions ++= Seq("-Ypartial-unification")
scalaVersion := "2.12.13"

// testing 3.1.0 release
resolvers += "daffodil-staging" at "https://repository.apache.org/content/repositories/orgapachedaffodil-1020/"

val zioVer = "1.0.6"
val daffodilVer = "3.1.0"
libraryDependencies := Seq(
  "com.microsoft.java" % "com.microsoft.java.debug.core" % "0.31.1",
  "co.fs2" %% "fs2-io" % "3.0.2",
  
  "dev.zio" %% "zio" % zioVer,
  "dev.zio" %% "zio-streams" % zioVer,
  
  "org.scalafx" %% "scalafx" % s"14-R19",
  "io.github.java-diff-utils" % "java-diff-utils" % "4.9",
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.13",
  "org.apache.daffodil" %% "daffodil-sapi" % daffodilVer,
  "org.apache.daffodil" %% "daffodil-runtime1" % daffodilVer,
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)


// portable config from the quickstart
// http://www.scalafx.org/docs/quickstart/
lazy val javaFXModules = {
  // Determine OS version of JavaFX binaries
  lazy val osName = System.getProperty("os.name") match {
    case n if n.startsWith("Linux")   => "linux"
    case n if n.startsWith("Mac")     => "mac"
    case n if n.startsWith("Windows") => "win"
    case _ =>
      throw new Exception("Unknown platform!")
  }
  // Create dependencies for JavaFX modules
  Seq("base", "controls", "fxml", "graphics", "media", "swing")
    .map(m => "org.openjfx" % s"javafx-$m" % "14.0.1" classifier osName)
}
libraryDependencies ++= javaFXModules

enablePlugins(GitVersioning, JavaAppPackaging, UniversalPlugin)
