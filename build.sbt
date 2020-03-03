ThisBuild / scalaVersion := "2.13.1"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xlint",
  "-Ymacro-annotations"
)

val commonSettings = Seq(
  Compile / console / scalacOptions += "-Ywarn-unused:-imports,_",
  Test / console / scalacOptions += "-Ywarn-unused:-imports,_",
  Compile / doc / scalacOptions ++= Seq("-diagrams", "-diagrams-max-classes", "10"),
  // resolvers += Resolver.sonatypeRepo("releases"),
  // resolvers += Resolver.sonatypeRepo("snapshots"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "labo",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    libraryDependencies += "eu.timepit" %% "singleton-ops" % "0.4.3",
    libraryDependencies += "codes.quine" %% "dali-core" % "0.2.0",
    libraryDependencies += "codes.quine" %% "dali-cats" % "0.2.0",
    libraryDependencies += "io.monix" %% "minitest" % "2.7.0" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    scalapropsSettings,
    scalapropsVersion := "0.6.1",
    addCompilerPlugin("com.github.ghik" % "silencer-plugin" % "1.6.0" cross CrossVersion.full),
    libraryDependencies += "com.github.ghik" % "silencer-lib" % "1.6.0" % Provided cross CrossVersion.full,
    commonSettings,
    initialCommands in console :=
      """
        |import codes.quine.labo._
        |import codes.quine.labo.data._
        |import codes.quine.labo.neko._
        |import codes.quine.labo.neko.syntax._
        |import codes.quine.labo.neko.data._
        |import codes.quine.labo.neko.instances._
      """.stripMargin
  )
  .dependsOn(`neko-core`, `neko-free`, `neko-rec`, `hollow`)
  .aggregate(`neko-core`, `neko-free`, `neko-rec`, `hollow`)

lazy val `neko-core` = project
  .in(file("modules/neko-core"))
  .settings(
    name := "neko-core",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0",
    libraryDependencies += "com.github.scalaprops" %% "scalaprops-core" % "0.6.3",
    scalapropsSettings,
    scalapropsVersion := "0.6.3",
    commonSettings
  )

lazy val `neko-free` = project
  .in(file("modules/neko-free"))
  .settings(
    name := "neko-free",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0",
    commonSettings
  )
  .dependsOn(`neko-core`)

lazy val `neko-rec` = project
  .in(file("modules/neko-rec"))
  .settings(
    name := "neko-rec",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0",
    commonSettings
  )
  .dependsOn(`neko-core`)

lazy val `hollow` = project
  .in(file("modules/hollow"))
  .settings(
    name := "hollow",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0",
    commonSettings,
    initialCommands in console :=
      """
        |import codes.quine.labo.hollow._
        |import codes.quine.labo.hollow.Parser._
      """.stripMargin
  )
  .dependsOn(`neko-core`)
