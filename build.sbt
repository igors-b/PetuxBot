name := "PETUX"

version := "0.1"

scalaVersion := "2.13.3"

val canoeVersion = "0.5.1"
val catsVersion = "2.3.0"
val enumeratumVersion = "1.7.0"


libraryDependencies ++= Seq(
  "org.augustjune" %% "canoe"      % canoeVersion,
  "com.beachape"   %% "enumeratum" % enumeratumVersion,
  "org.typelevel"  %% "cats-core"  % catsVersion
  )