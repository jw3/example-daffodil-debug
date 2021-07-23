name := "daffodil-debugger"

git.useGitDescribe := true

javacOptions ++= Seq("-source", "11")
scalacOptions ++= Seq("-Ypartial-unification")
scalaVersion := "2.12.13"

val zioVer = "1.0.6"
val daffodilVer = "3.1.0"
libraryDependencies := Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.microsoft.java" % "com.microsoft.java.debug.core" % "0.31.1",
  "co.fs2" %% "fs2-io" % "3.0.4",
  "com.monovore" %% "decline-effect" % "2.1.0",
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.13",
  "dev.zio" %% "zio" % zioVer,
  "dev.zio" %% "zio-streams" % zioVer,
  "io.github.java-diff-utils" % "java-diff-utils" % "4.9",
  "org.apache.daffodil" %% "daffodil-sapi" % daffodilVer,
  "org.apache.daffodil" %% "daffodil-runtime1" % daffodilVer,
  "org.scalafx" %% "scalafx" % s"14-R19",
  "org.typelevel" %% "log4cats-slf4j" % "2.1.0",
)

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, "daffodilVersion" -> daffodilVer)
buildInfoPackage := "ddb.debugger"

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

enablePlugins(BuildInfoPlugin, GitVersioning, JavaAppPackaging, UniversalPlugin)
