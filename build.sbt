name := "fsis"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum"  % "0.12.0"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
