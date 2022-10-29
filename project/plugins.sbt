addSbtPlugin("org.scala-js"                % "sbt-scalajs"         % "1.11.0")
addSbtPlugin("ch.epfl.scala"               % "sbt-scalajs-bundler" % "0.20.0")
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter"       % "1.0.0-beta39")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.3.1")

// for reading npmDependencies from package.json
libraryDependencies ++= Seq("com.lihaoyi" %% "upickle" % "2.0.0")
