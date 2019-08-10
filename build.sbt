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
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "labo",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M4",
    scalapropsSettings,
    scalapropsVersion := "0.6.0",
    commonSettings
  )
  .dependsOn(neko, free)
  .aggregate(neko, free)

lazy val neko = project
  .in(file("modules/neko"))
  .settings(
    name := "neko",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0",
    libraryDependencies += "com.github.scalaprops" %% "scalaprops-core" % "0.6.0",
    scalapropsSettings,
    scalapropsVersion := "0.6.0",
    commonSettings
  )

lazy val free = project
  .in(file("modules/neko-free"))
  .settings(
    name := "neko-free",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0",
    commonSettings
  )
  .dependsOn(neko)
