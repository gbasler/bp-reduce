package bpReduce
package transformations

import bpReduce.ast.Function
import bpReduce.ast.Stmt._

object TargetCollector {
  /**
   * @param function
   * @return All labels that are targets of gotos etc.
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
