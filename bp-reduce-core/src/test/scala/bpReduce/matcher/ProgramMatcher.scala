package bpReduce
package matcher

import bpReduce.ast.Program
import org.specs2.matcher.{Expectable, Matcher}
import bpReduce.writer.Formatter

/**
 * The only purpose of this matcher is to have a nicer
 * diff in case the unit tests fail.
 * It does nothing more than `===`...
 *
 * @param program
 */
class ProgramMatcher(program: Program) extends Matcher[Program] {
  def apply[S <: Program](n: Expectable[S]) = {

    import Formatter.format

    result(program == n.value,
      s"\n${format(n.value)} is equal to\n${format(program)}",
      s"\n${format(n.value)} is not equal to\n${format(program)}", n)
  }
}