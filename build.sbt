name := "fp-in-scala"
version := "0.1"
scalaVersion := "2.13.4"



libraryDependencies ++= Seq (
  "org.typelevel" %% "cats-core" % "2.2.0"
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
  "-Wself-implicit",
  //"-Vimplicits"
)
