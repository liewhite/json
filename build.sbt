ThisBuild / organization := "io.github.liewhite"
ThisBuild / organizationName := "liewhite"
ThisBuild / version := sys.env.get("RELEASE_VERSION").getOrElse("0.4.2")
ThisBuild / scalaVersion := "3.2.0"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / publishTo := sonatypePublishToBundle.value
sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

lazy val root = project
  .in(file("."))
  .settings(
    name := "json",
    libraryDependencies += "io.github.liewhite" %% "common" % "0.0.3",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "shapeless3-deriving" % "3.2.0"
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.3",
      "io.circe" %% "circe-parser" % "0.14.3",
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test", 
  )
