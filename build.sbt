Global / onChangedBuildSource := IgnoreSourceChanges // not working well with webpack devserver

name                     := "Codingamemarslander"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.8"

val versions = new {
  val outwatch           = "1.0.0-RC7"
  val funPack            = "0.2.0"
  val scalaTest          = "3.2.12"
  val circeVersion       = "0.14.1"
  val sttpClient3Version = "3.6.2"
  val catsEffectVersion  = "3.3.12"
}

lazy val scalaJsMacrotaskExecutor = Seq(
  // https://github.com/scala-js/scala-js-macrotask-executor
  libraryDependencies       += "org.scala-js" %%% "scala-js-macrotask-executor" % "1.0.0",
  Compile / npmDependencies += "setimmediate"  -> "1.0.5", // polyfill
)

lazy val simulator = project
  .in(file("simulator"))
  .enablePlugins(
    ScalaJSPlugin,
    ScalaJSBundlerPlugin,
  )
  //  .settings(scalaJsMacrotaskExecutor)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest"     % versions.scalaTest % Test,
      "io.circe"      %%% "circe-generic" % versions.circeVersion,
      "io.circe"      %%% "circe-parser"  % versions.circeVersion,
      "org.typelevel" %%% "cats-effect"   % versions.catsEffectVersion,
    ),
    scalacOptions --= Seq(
      "-Xfatal-warnings",
    ), // overwrite option from https://github.com/DavidGregory084/sbt-tpolecat
  )

lazy val bot = project
  .in(file("bot"))
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.8.1",
    ),
  )

lazy val cli = project
  .in(file("cli"))
  .enablePlugins(
    ScalaJSPlugin,
    ScalaJSBundlerPlugin,
    ScalablyTypedConverterPlugin,
  )
  .dependsOn(simulator)
  .settings(
//    Compile / npmDevDependencies   ++= Seq(
//      "@fun-stack/fun-pack" -> versions.funPack, // sane defaults for webpack development and production, see webpack.config.*.js
//    ),
    scalacOptions --= Seq(
      "-Xfatal-warnings",
    ), // overwrite option from https://github.com/DavidGregory084/sbt-tpolecat
    useYarn                         := true, // Makes scalajs-bundler use yarn instead of npm
    scalaJSUseMainModuleInitializer := true,
    webpackConfigFile               := Some(baseDirectory.value / "webpack.config.js"),
    Compile / npmDependencies      ++= Seq(
      "@types/node" -> "16.11.7",
    ),
  )

lazy val webapp = project
  .in(file("webapp"))
  .enablePlugins(
    ScalaJSPlugin,
    ScalaJSBundlerPlugin,
  )
  .dependsOn(simulator)
  .settings(scalaJsMacrotaskExecutor)
  .settings(
    libraryDependencies          ++= Seq(
      "io.github.outwatch"            %%% "outwatch"      % versions.outwatch,
      "org.scalatest"                 %%% "scalatest"     % versions.scalaTest % Test,
      "io.circe"                      %%% "circe-generic" % versions.circeVersion,
      "io.circe"                      %%% "circe-parser"  % versions.circeVersion,
      "com.softwaremill.sttp.client3" %%% "circe"         % versions.sttpClient3Version,
      "com.softwaremill.sttp.client3" %%% "cats"          % versions.sttpClient3Version,
    ),
    Compile / npmDevDependencies ++= Seq(
      "@fun-stack/fun-pack" -> versions.funPack, // sane defaults for webpack development and production, see webpack.config.*.js
    ),
    scalacOptions --= Seq(
      "-Xfatal-warnings",
    ), // overwrite option from https://github.com/DavidGregory084/sbt-tpolecat

    useYarn := true, // Makes scalajs-bundler use yarn instead of npm
    scalaJSLinkerConfig ~= (_.withModuleKind(
      ModuleKind.CommonJSModule,
    )), // configure Scala.js to emit a JavaScript module instead of a top-level script
    scalaJSUseMainModuleInitializer   := true, // On Startup, call the main function
    webpackDevServerPort              := 12345,
    webpack / version                 := "4.46.0",
    startWebpackDevServer / version   := "3.11.3",
    webpackDevServerExtraArgs         := Seq("--color"),
    fullOptJS / webpackEmitSourceMaps := true,
    fastOptJS / webpackBundlingMode   := BundlingMode
      .LibraryOnly(), // https://scalacenter.github.io/scalajs-bundler/cookbook.html#performance
    fastOptJS / webpackConfigFile := Some(baseDirectory.value / "webpack.config.dev.js"),
    fullOptJS / webpackConfigFile := Some(baseDirectory.value / "webpack.config.prod.js"),
    Test / requireJsDomEnv        := true,
  )

addCommandAlias("prod", "fullOptJS/webpack")
addCommandAlias("dev", "devInit; devWatchAll; devDestroy")
addCommandAlias("devInit", "; webapp/fastOptJS/startWebpackDevServer")
addCommandAlias("devWatchAll", "~; webapp/fastOptJS/webpack")
addCommandAlias("devDestroy", "webapp/fastOptJS/stopWebpackDevServer")
