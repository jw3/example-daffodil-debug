name := "daffodil-debugger"

version := "0.1"

scalaVersion := "2.12.13"

val zioVer = "1.0.6"
val daffodilVer = "3.1.0-SNAPSHOT"
libraryDependencies := Seq(
  "dev.zio" %% "zio" % zioVer,
  "dev.zio" %% "zio-streams" % zioVer,
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.13",
  "org.apache.daffodil" %% "daffodil-sapi" % daffodilVer,
  "org.apache.daffodil" %% "daffodil-runtime1" % daffodilVer,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
)

dependsOn(ProjectRef(uri("git://github.com/DFDLSchemas/JPEG.git"), "jpeg"))
