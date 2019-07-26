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
for {
  project <- Seq(root, neko, free)
  scope <- Seq(Compile, Test)
} yield project / scope / console / scalacOptions += "-Ywarn-unused:-imports,_"
ThisBuild / resolvers += Resolver.sonatypeRepo("releases")

lazy val root = project.in(file("."))
  .settings(
    name := "labo",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  )
  .dependsOn(neko, free)
  .aggregate(neko, free)

lazy val neko = project.in(file("modules/neko"))
  .settings(
    name := "neko",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0",
    libraryDependencies += "com.github.scalaprops" %% "scalaprops-core" % "0.6.0",
    scalapropsSettings,
    scalapropsVersion := "0.6.0",
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  )

lazy val free = project.in(file("modules/neko-free"))
  .settings(
    name := "neko-free",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0",
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  )
  .dependsOn(neko)
