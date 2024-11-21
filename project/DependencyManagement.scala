import sbt._

object DependencyManagement {

  /** Excluded and version managed transitive artifacts */
  def ivyExclusionsAndOverrides =
    <dependencies>
      <exclude org="org.junit" rev="*"/>
      <exclude org="org.junit" rev="4.8.1"/>
      <exclude org="colt" rev="1.2.0"/>
      <exclude org="blas" module="blas" rev="*"/>
      <exclude org="javax.script" rev="*"/>
      <exclude org="org.scala-tools.testing" module="scalacheck_2.8.0" rev="1.7"/>
      <override org="junit" module="junit" rev="4.8.1"/>
      <exclude org="org.eclipse" rev="*"/>
    </dependencies>

  /** Utilities for File IO */
  def CommonsIo = "commons-io" % "commons-io" % "2.4"

  /** General utilities for Java language */
  def CommonsLang = "commons-lang" % "commons-lang" % "2.6"

  /** General utilities for Java language */
  def CommonsExec = "org.apache.commons" % "commons-exec" % "1.3"

  def SpringCore = "org.springframework" % "spring-core" % "4.1.4.RELEASE"

  /**
   * Specs, unit testing framework
   *
   * http://code.google.com/p/specs/
   */
  object Specs {
    def SpecsModule(name: String) = {
      "org.specs2" %% name % "4.19.2" % "test"
    }

    lazy val dependencies = Seq(
      SpecsModule("specs2-core"),
      SpecsModule("specs2-matcher-extra"),
      SpecsModule("specs2-scalacheck"),
      SpecsModule("specs2-junit")
    )
  }

  def JUnit = "junit" % "junit" % "4.12" % "test" intransitive()

  /**
   * Scalacheck, automated unit testing using randomized test cases.
   *
   * http://code.google.com/p/scalacheck/
   *
   * We use this through Specs.
   */
  def Scalacheck = "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"

  /** Dependency of Specs */
  def MockitoAll = "org.mockito" % "mockito-all" % "1.10.19" % "test"

  //  def TreeHugger = "com.eed3si9n" % "treehugger_2.9.1" % "0.2.1"
  def TreeHugger = "com.eed3si9n" % "treehugger_2.9.1" % "0.3.0"

  /** Date and Time represenation */
  def JodaTime = "joda-time" % "joda-time" % "2.6"

  def JodaConvert = "org.joda" % "joda-convert" % "1.7"

  def ScalaMeter = "com.github.axel22" %% "scalameter" % "0.3"

  /**
   * Collections specialized for primitive elements
   * http://fastutil.dsi.unimi.it/
   */
  def Fastutil = "it.unimi.dsi" % "fastutil" % "6.5.16"


  /**
   * kiama A Scala library for language processing
   *
   * http://code.google.com/p/kiama/
   */
  def Kiama = "com.googlecode.kiama" %% "kiama" % "1.8.0"

  /**
   * Scopt, command line parsing
   */
  def Scopt = "com.github.scopt" %% "scopt" % "3.7.1"

  def ScalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
}
