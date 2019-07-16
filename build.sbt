ThisBuild / scalaVersion := "2.13.0"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xlint",
  "-Ymacro-annotations",
)
ThisBuild / resolvers += Resolver.sonatypeRepo("releases")

lazy val root = project.in(file("."))
  .settings(
    name := "labo",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  )
  .dependsOn(nyan, free)
  .aggregate(nyan, free)

lazy val nyan = project.in(file("modules/nyan"))
  .settings(
    name := "nyan",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0",
    libraryDependencies += "com.github.scalaprops" %% "scalaprops-core" % "0.6.0",
    scalapropsSettings,
    scalapropsVersion := "0.6.0",
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  )

lazy val free = project.in(file("modules/nyan-free"))
  .settings(
    name := "nyan-free",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0",
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  )
  .dependsOn(nyan)
