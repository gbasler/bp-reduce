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
  def CommonsLang = "commons-lang" % "commons-lang" % "2.4"

  /** Scala library providing Actors and Promises (for concurrency), and functional programming tools */
  def ScalazCore = "org.scalaz" %% "scalaz-core" % "7.0.0"

  def ScalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % "7.0.0"

  def SpringCore = "org.springframework" % "spring-core" % "2.5.6"

  /**
   * Specs, unit testing framework
   *
   * http://code.google.com/p/specs/
   */
  def Specs = "org.specs2" %% "specs2" % "2.0" % "test" intransitive()

  def JUnit = "junit" % "junit" % "4.8.1" % "test" intransitive()

  /**
   * Scalacheck, automated unit testing using randomized test cases.
   *
   * http://code.google.com/p/scalacheck/
   *
   * We use this through Specs.
   */
  def Scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

  /** Dependency of Specs */
  def MockitoAll = "org.mockito" % "mockito-all" % "1.8.4" % "test"

  //  def TreeHugger = "com.eed3si9n" % "treehugger_2.9.1" % "0.2.1"
  def TreeHugger = "com.eed3si9n" % "treehugger_2.9.1" % "0.1.0"

  /** Date and Time represenation */
  def JodaTime = "joda-time" % "joda-time" % "2.2"

  def JodaConvert = "org.joda" % "joda-convert" % "1.2"

  def ScalaMeter = "com.github.axel22" %% "scalameter" % "0.3"

  /**
   * Collections specialized for primitive elements
   * http://fastutil.dsi.unimi.it/
   */
  def Fastutil = "it.unimi.dsi" % "fastutil" % "6.5.9"

}
