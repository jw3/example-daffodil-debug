name := "daffodil-debugger"

version := "0.1"

scalaVersion := "2.12.13"

val daffodilVer = "3.1.0-SNAPSHOT"
libraryDependencies := Seq(
  "org.apache.daffodil" %% "daffodil-sapi" % daffodilVer,
  "org.apache.daffodil" %% "daffodil-runtime1" % daffodilVer,
)

dependsOn(ProjectRef(uri("git://github.com/DFDLSchemas/JPEG.git"), "jpeg"))
