name := "DM Forms"

normalizedName := "dm-forms"

description := "an alternative forms framework for Play (Scala)"

organization := "org.dupontmanual"

organizationName := "duPont Manual High School"

version := "0.1"

scalaVersion := "2.10.2"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
    "org.webjars" % "webjars-play_2.10" % "2.1.0-3",
    "javax.mail" % "mail" % "1.4.7",
    "com.scalatags" % "scalatags_2.10" % "0.1.2",
    "org.webjars" % "jquery" % "2.0.0",
    "org.webjars" % "bootstrap" % "2.3.2",
    "org.webjars" % "jquery-ui" % "1.10.2-1",
    "org.webjars" % "bootstrap-datepicker" % "1.0.1",
    "org.webjars" % "bootstrap-timepicker" % "0.2.3",
    "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"
)
