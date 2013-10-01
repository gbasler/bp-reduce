import sbt._
import Keys._

object build extends Build {

  import DependencyManagement._

  // Settings shared by all sub-projects.
  val standardSettings: Seq[Project.Setting[_]] =
    Seq[Project.Setting[_]](
      ivyXML := DependencyManagement.ivyExclusionsAndOverrides,
      scalaVersion := "2.10.2",
      resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
        "releases" at "http://scala-tools.org/repo-releases")
    )

  //
  // PROJECTS
  //

  // Parent Project, it aggregates all others.
  lazy val bpReduce = Project(
    id = "bp-reduce",
    base = file("."),
    settings = Defaults.defaultSettings ++ standardSettings,
    aggregate = Seq[ProjectReference](bpReduceCore)
  )

  lazy val bpReduceCore = Project(
    id = "bp-reduce-core",
    base = file("bp-reduce-core"),
    settings = Defaults.defaultSettings ++ standardSettings ++ Seq(
      libraryDependencies ++= Seq(ScalazCore, ScalazConcurrent, Specs, JUnit, Scalacheck, MockitoAll, CommonsIo,
        TreeHugger, JodaConvert, JodaTime, CommonsLang, Fastutil)
    )
  )

}
