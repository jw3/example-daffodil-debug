name := "daffodil-debugger"

version := "0.1"

javacOptions ++= Seq("-source", "11")
scalaVersion := "2.12.13"

val zioVer = "1.0.6"
val daffodilVer = "3.1.0-SNAPSHOT"
libraryDependencies := Seq(
  "dev.zio" %% "zio" % zioVer,
  "dev.zio" %% "zio-streams" % zioVer,
  "org.scalafx" %% "scalafx" % s"14-R19",
  "io.github.java-diff-utils" % "java-diff-utils" % "4.9",
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.13",
  "org.apache.daffodil" %% "daffodil-sapi" % daffodilVer,
  "org.apache.daffodil" %% "daffodil-runtime1" % daffodilVer,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
)

lazy val jfx = Seq("base", "controls", "fxml", "graphics", "media", "swing")
libraryDependencies ++= jfx.map(mod => "org.openjfx" % s"javafx-$mod" % "14.0.1" classifier "linux")

dependsOn(ProjectRef(uri("git://github.com/DFDLSchemas/JPEG.git"), "jpeg"))
