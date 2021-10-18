name := "fp-in-scala"
version := "0.1"
scalaVersion := "2.13.5"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

libraryDependencies ++= Seq (
  "org.typelevel"   %% "cats-core"           % "2.6.1",
  "org.typelevel"   %% "cats-effect"         % "3.2.9",
  "co.fs2"          %% "fs2-core"            % "3.1.5",
  "co.fs2"          %% "fs2-io"              % "3.1.5",
  "org.http4s"      %% "http4s-dsl"          % "1.0.0-M23",
  "org.http4s"      %% "http4s-blaze-server" % "1.0.0-M23",
  "org.http4s"      %% "http4s-blaze-client" % "1.0.0-M23",

  "io.github.vigoo" %% "prox-fs2-3"          % "0.7.3",
  "org.tpolecat"    %% "doobie-core"         % "1.0.0-M2",

  "io.circe"        %% "circe-core"          % "0.14.1",
  "io.circe"        %% "circe-generic"       % "0.14.1",
  "io.circe"        %% "circe-parser"        % "0.14.1",
  "is.cir"          %% "ciris"               % "2.1.1",
  "is.cir"          %% "ciris-enumeratum"    % "2.1.1",
  "is.cir"          %% "ciris-circe"         % "2.1.1",

  "com.outr"        %% "scribe-slf4j"        % "3.5.5",

  "com.lihaoyi"     %% "ammonite-ops"        % "2.3.8",

  "org.apache.jena" % "apache-jena-libs"     % "4.1.0" pomOnly(),
  "org.topbraid"    % "shacl"                % "1.3.2",

  "com.github.imrafaelmerino" %% "json-scala-values" % "4.0.0",
  "org.json"                  % "json"               % "20210307",

  "com.oracle.database.jdbc"  % "ojdbc8"             % "12.2.0.1",


  "org.scalacheck"            %% "scalacheck"        % "1.15.4",




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
