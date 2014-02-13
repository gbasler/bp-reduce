package bpReduce.matcher

import bpReduce.ast.Program
import org.specs2.matcher.{Expectable, Matcher}
import bpReduce.writer.Formatter

class ProgramMatcher(program: Program) extends Matcher[Program] {
  def apply[S <: Program](n: Expectable[S]) = {

    import Formatter.format

    result(program == n.value,
      format(n.value) + " is equal to " + format(program),
      format(n.value) + " is not equal to " + format(program), n)
  }
}