package bpReduce.matcher

import bpReduce.ast.Program
import org.specs2.matcher.{Expectable, Matcher}
import bpReduce.writer.Formatter

class ProgramMatcher(program: Program) extends Matcher[Program] {
  def apply[S <: Program](n: Expectable[S]) = {

    import Formatter.format

    result(program == n.value,
      s"${format(n.value)} is equal to\n${format(program)}",
      s"${format(n.value)} is not equal to\n${format(program)}", n)
  }
}