ThisBuild / version := "0.1"
ThisBuild / organization := "io.github.oybek"

val settings = Compiler.settings ++ Seq()

lazy val playcs = (project in file("."))
  .settings(name := "playcs")
  .settings(libraryDependencies ++= Dependencies.common)

lazy val api = ProjectRef(uri("https://github.com/oybek/telegramium2.git#master"), "telegramium-core")
lazy val high = ProjectRef(uri("https://github.com/oybek/telegramium2.git#master"), "telegramium-high")
lazy val root = project.in(file(".")).dependsOn(api, high)
