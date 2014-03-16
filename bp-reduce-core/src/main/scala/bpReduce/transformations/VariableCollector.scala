package bpReduce
package transformations

import bpReduce.ast.Function
import bpReduce.ast.Stmt._

object VariableCollector {
  /**
   * @param function
   * @return All variables that are read / written by a statement.
   */
  def apply(function: Function): Set[String] = {
    function.collect {
      case goto@Goto(targets)       =>
        targets
      case start@StartThread(label) =>
        Seq(label)
    }.flatten.toSet
  }
}
