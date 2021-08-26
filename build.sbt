name := "fp-in-scala"
version := "0.1"
scalaVersion := "2.13.5"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

libraryDependencies ++= Seq (
  "org.typelevel"   %% "cats-core"         % "2.4.2",
  "org.typelevel"   %% "cats-effect"       % "3.0.0",
  "org.apache.jena" % "apache-jena-libs"   % "4.0.0" pomOnly(),
  "org.topbraid"    % "shacl"              % "1.3.2",
  "com.beachape"    %% "enumeratum"        % "1.6.0",
  "com.lihaoyi"     %% "ammonite-ops"      % "2.3.8",
  "com.outr"        %% "scribe-slf4j"      % "3.0.4",
  "co.fs2"          %% "fs2-core"          % "3.0.4",
  "co.fs2"          %% "fs2-io"            % "3.0.4",
  "org.tpolecat"    %% "doobie-core"       % "1.0.0-M2",


  "org.http4s"      %% "http4s-dsl"          % "1.0.0-M23",
  "org.http4s"      %% "http4s-blaze-server" % "1.0.0-M23",
  "org.http4s"      %% "http4s-blaze-client" % "1.0.0-M23",


  "com.oracle.database.jdbc"  % "ojdbc8"      % "12.2.0.1",


  "org.scalacheck"            %% "scalacheck"        % "1.15.4",

  "com.github.imrafaelmerino" %% "json-scala-values" % "4.0.0",

  "org.json" % "json" % "20210307"


  )

scalacOptions := Seq(
  "-feature",
  "-explaintypes",
  "-deprecation",
  "-Xlint:valpattern",
  "-language:higherKinds",
  //"-Xlint:missing-interpolator",
  "-Xlint:stars-align",
  "-Yrangepos",
  "-Xlint:implicit-not-found",
  "-Wextra-implicit",
  "-Wunused:implicits",
  "-Wunused:locals"
  //"-g:notailcalls"
  //"-Wself-implicit",
  //"-Vimplicits"
)
