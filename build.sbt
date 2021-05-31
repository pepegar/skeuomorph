ThisBuild / organization := "io.higherkindness"
ThisBuild / githubOrganization := "47degrees"
ThisBuild / scalaVersion := "2.13.4"
ThisBuild / crossScalaVersions := Seq("2.12.12", "2.13.2")

addCommandAlias("ci-test", "scalafmtCheckAll; scalafmtSbtCheck; microsite/mdoc; testCovered")
addCommandAlias("ci-docs", "github; documentation/mdoc; headerCreateAll; microsite/publishMicrosite")
addCommandAlias("ci-publish", "github; ci-release")

lazy val skeuomorph = project
  .in(file("."))
  .settings(commonSettings)
  .settings(moduleName := "skeuomorph")

lazy val microsite = project
  .dependsOn(skeuomorph)
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(mdocSettings)
  .settings(
    micrositeName := "Skeuomorph",
    micrositeDescription := "Skeuomorph is a library for transforming different schemas in Scala",
    micrositeBaseUrl := "/skeuomorph",
    micrositeHighlightTheme := "tomorrow",
    micrositeDocumentationUrl := "docs",
    includeFilter in Jekyll := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md" | "*.svg",
    micrositeGithubToken := Option(System.getenv().get("GITHUB_TOKEN")),
    micrositePushSiteWith := GitHub4s,
    micrositeFavicons := Seq(microsites.MicrositeFavicon("favicon.png", "32x32")),
    micrositePalette := Map(
      "brand-primary"   -> "#4A00D8",
      "brand-secondary" -> "#FC00CD",
      "white-color"     -> "#FFF"
    ),
    micrositeExtraMdFiles := Map(
      file("CHANGELOG.md") -> microsites.ExtraMdFileConfig(
        "changelog.md",
        "docs",
        Map("title" -> "changelog", "permalink" -> "docs/changelog/")
      )
    )
  )
  .enablePlugins(MicrositesPlugin)

lazy val documentation = project
  .settings(mdocOut := file("."))
  .settings(publish / skip := true)
  .enablePlugins(MdocPlugin)

// General Settings
lazy val commonSettings = Seq(
  scalacOptions ~= (_ filterNot Set("-Xfuture", "-Xfatal-warnings").contains),
  libraryDependencies ++= Seq(
    "org.typelevel"        %% "cats-core"         % "2.6.1",
    "org.typelevel"        %% "cats-effect"       % "2.5.1",
    "io.higherkindness"    %% "droste-core"       % "0.8.0",
    "io.higherkindness"    %% "droste-macros"     % "0.8.0",
    "org.apache.avro"       % "avro"              % "1.10.2",
    "com.github.os72"       % "protoc-jar"        % "3.11.4",
    "com.google.protobuf"   % "protobuf-java"     % "3.17.0",
    "io.circe"             %% "circe-core"        % "0.14.1",
    "io.circe"             %% "circe-parser"      % "0.14.1",
    "io.circe"             %% "circe-yaml"        % "0.13.1",
    "org.scalameta"        %% "scalameta"         % "4.4.19",
    "com.julianpeeters"    %% "avrohugger-core"   % "1.0.0-RC24" % Test,
    "org.typelevel"        %% "cats-laws"         % "2.6.1"      % Test,
    "io.circe"             %% "circe-testing"     % "0.14.1"     % Test,
    "org.typelevel"        %% "discipline-specs2" % "1.1.6"      % Test,
    "org.specs2"           %% "specs2-core"       % "4.12.0"     % Test,
    "org.specs2"           %% "specs2-scalacheck" % "4.12.0"     % Test,
    "org.scalacheck"       %% "scalacheck"        % "1.15.4"     % Test,
    "io.chrisdavenport"    %% "cats-scalacheck"   % "0.3.0"      % Test,
    "org.scalatra.scalate" %% "scalate-core"      % "1.9.6"      % Test
  )
) ++ compilerPlugins ++ macroSettings

lazy val mdocSettings = Seq(
  scalacOptions ~= filterConsoleScalacOptions,
  scalacOptions ~= (_ filterNot Set("-Xfatal-warnings", "-Ywarn-unused-import", "-Xlint").contains)
)

lazy val compilerPlugins = Seq(
  libraryDependencies ++= Seq(
    compilerPlugin("org.typelevel" % "kind-projector"     % "0.11.2" cross CrossVersion.full),
    compilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1")
  )
)

lazy val macroSettings: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided
  ),
  libraryDependencies ++= on(2, 12)(
    compilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full)
  ).value,
  scalacOptions ++= on(2, 13)("-Ymacro-annotations").value
)

def on[A](major: Int, minor: Int)(a: A): Def.Initialize[Seq[A]] =
  Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some(v) if v == (major, minor) => Seq(a)
      case _                              => Nil
    }
  }
