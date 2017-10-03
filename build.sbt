//scalaVersion := "2.12.4-bin-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-Xlog-implicits"
)
