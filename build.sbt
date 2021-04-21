name := "fp-in-scala"
version := "0.1"
scalaVersion := "2.13.5"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

libraryDependencies ++= Seq (
  "org.typelevel" %% "cats-core" % "2.4.2",
  "org.typelevel" %% "cats-effect" % "3.0.0",
  "org.apache.jena" % "apache-jena-libs" % "4.0.0" pomOnly(),
  "com.beachape" %% "enumeratum" % "1.6.0",
  "com.lihaoyi" %% "ammonite-ops" % "2.3.8",
  "com.outr" %% "scribe-slf4j" % "3.4.0"


)

scalacOptions := Seq(
  "-feature",
  "-explaintypes",
  "-deprecation",
  "-Xlint:valpattern",
  "-language:higherKinds",
  "-Xlint:missing-interpolator",
  "-Xlint:stars-align",
  "-Yrangepos",
  "-Xlint:implicit-not-found",
  "-Wextra-implicit",
  "-Wunused:implicits",
  //"-g:notailcalls"
  //"-Wself-implicit",
  //"-Vimplicits"
)
