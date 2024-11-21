import sbt.{Compile, Def, _}
import sbt.Keys.skip
import DependencyManagement._

// Settings shared by all sub-projects.
val standardSettings: Seq[Def.Setting[_]] =
  Seq[Def.Setting[_]](
    ivyXML := DependencyManagement.ivyExclusionsAndOverrides,
    scalaVersion := "2.13.15",
    resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
      "releases" at "http://scala-tools.org/repo-releases")
  )

//
// PROJECTS
//

// Parent Project, it aggregates all others.
lazy val bpReduce = Project(
  id = "bp-reduce",
  base = file("."))
  .settings(standardSettings)
  .aggregate(bpReduceCore)

lazy val bpReduceCore = Project(
  id = "bp-reduce-core",
  base = file("bp-reduce-core"))
  .settings(standardSettings)
  .settings(
    libraryDependencies ++= Seq(JUnit, MockitoAll, CommonsIo, ScalaParserCombinators,
      TreeHugger, JodaConvert, JodaTime, CommonsLang, Fastutil, CommonsExec, Scopt,
      SpringCore % "test") ++ Specs.dependencies
  )

