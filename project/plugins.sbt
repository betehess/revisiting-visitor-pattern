//resolvers += "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"

// addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.0.10")

libraryDependencies +=
 Defaults.sbtPluginExtra(
   "org.ensime" % "ensime-sbt-cmd" % "0.0.10",
   "0.11.2",
   "2.9.1"
 )
