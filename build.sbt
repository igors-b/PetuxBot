name := "PETUXBOT"

version := "0.1"

scalaVersion := "2.13.3"

val canoeVersion = "0.5.1"
val catsVersion = "2.3.0"
val catsEffectVersion = "2.5.3"
val scalaTestVersion = "3.2.10"


libraryDependencies ++= Seq(
  "org.augustjune" %% "canoe"       % canoeVersion,
  "org.scalatest"  %% "scalatest"   % scalaTestVersion  %  Test,
  "org.typelevel"  %% "cats-core"   % catsVersion,
  "org.typelevel"  %% "cats-effect" % catsEffectVersion
  )