ThisBuild / scalaVersion := "2.13.0"
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
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "labo",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    libraryDependencies += "codes.quine" %% "dali-core" % "0.1.0",
    libraryDependencies += "codes.quine" %% "dali-cats" % "0.1.0",
    scalapropsSettings,
    scalapropsVersion := "0.6.1",
    addCompilerPlugin(("com.github.ghik" % "silencer-plugin" % "1.4.3").cross(CrossVersion.full)),
    libraryDependencies += ("com.github.ghik" % "silencer-lib" % "1.4.3" % Provided).cross(CrossVersion.full),
    commonSettings,
  )
  .dependsOn(nekoCore, nekoFree)
  .aggregate(nekoCore, nekoFree)

lazy val nekoCore = project
  .in(file("modules/neko-core"))
  .settings(
    name := "neko-core",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0",
    libraryDependencies += "com.github.scalaprops" %% "scalaprops-core" % "0.6.1",
    scalapropsSettings,
    scalapropsVersion := "0.6.1",
    commonSettings
  )

lazy val nekoFree = project
  .in(file("modules/neko-free"))
  .settings(
    name := "neko-free",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0",
    commonSettings
  )
  .dependsOn(nekoCore)
