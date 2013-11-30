name := "dM Forms"

normalizedName := "dm-forms"

description := "an alternative forms framework for Play (Scala)"

organization := "org.dupontmanual"

organizationName := "duPont Manual High School"

version := "0.2-SNAPSHOT"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"com.typesafe.play" %% "play" % "2.2.1",
    "org.webjars" %% "webjars-play" % "2.2.1",
    "javax.mail" % "mail" % "latest.release",
    "org.webjars" % "jquery" % "2.0.0",
    "org.webjars" % "bootstrap" % "2.3.2",
    "org.webjars" % "jquery-ui" % "1.10.2-1",
    "org.webjars" % "bootstrap-datepicker" % "1.0.1",
    "org.webjars" % "bootstrap-timepicker" % "0.2.3",
    "org.webjars" % "jquery-maskedinput" % "1.3.1",
    "org.scalatest" %% "scalatest" % "latest.release" % "test"
)

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

credentials += Credentials(Path.userHome / ".ssh" / ".credentials")

pomExtra := (
  <url>http://dupontmanual.github.io/dm-forms</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
    </license>
  </licenses>
  <scm>
    <url>git://github.com/dupontmanual/dm-forms.git</url>
    <connection>scm:git://github.com/dupontmanual/dm-forms.git</connection>
  </scm>
  <developers>
    <developer>
      <name>Allen Boss</name>
      <roles>
        <role>Student, Class of 2013</role>
      </roles>
    </developer>
    <developer>
      <name>Todd O'Bryan</name>
      <roles>
        <role>Teacher</role>
      </roles>
    </developer>
  </developers>
)