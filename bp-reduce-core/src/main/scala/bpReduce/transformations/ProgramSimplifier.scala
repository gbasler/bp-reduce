package bpReduce
package transformations

import bpReduce.ast._
import bpReduce.ast.Stmt.{Assign, Skip}
import bpReduce.ast.Expr.{True, Var}
import bpReduce.ast.Function
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.LabelledStmt
import bpReduce.ast.Program

/**
 * Checking a program is a very expensive operation.
 * Thus in order to arrive as quickly as possible at the most reduced program
 * (preserving the property that causes the bug in the model checker),
 * the calls to the model checker should be minimized.
 *
 * One method to reduce the number of calls is to check only one variant of a class of
 * programs (e.g., assuming that the all either have the property or they all don't have it).
 * A possible way of grouping the programs is to simplify them in such a way that the simplifications do not
 * alter the outcome.
 *
 * <ul>
 * <li>We assume that expressions can be simplified. The reason is that a simplified and non-simplified
 * expression results (for a BDD-based model checker) in exactly the same transition function.</li>
 * <li>`skip` statements do not alter the outcome as well. They only modify the program counter but
 * no variable. Thus the model checker does not alter any BDD's / SAT formulas. The program counter
 * logic is most likely correct since the model checker would most likely fail on many other programs.
 * </li>
 * </ul>
 *
 * These assumptions of course do not need to hold in any case but in practice I've never seen the contrary.
 */
object ProgramSimplifier {

  final case class Block(stmt: List[Stmt], labels: Seq[String])

  final case class FunctionUnderSimplification(blocks: Seq[Block])

  def apply(program: Program): Program = {
    val functions = for {
      function <- program.functions
      simplified = simplifyStmts(function)
      noSkips = removeNonTargetSkips(simplified)
    } yield noSkips
    program.copy(functions = functions)
  }

  /**
   * Very very conservative algorithm:
   * Removes all skips but the ones that are targets of gotos etc.
   * Most likely a more aggressive algorithm would work too but
   * the conservative algorithm is easier to implement and produces acceptable
   * results.
   *
   * TODO: check how much improvement a more aggressive algorithm would bring
   *
   * @param function
   */
  private def removeNonTargetSkips(function: Function) = {
    // if a skip has a label that is used, it will not be removed
    val usedLabels: Set[String] = TargetCollector(function)
    val stmts = function.stmts.filterNot {
      stmt =>
        stmt.stmt == Skip && stmt.labels.forall(label => !usedLabels.contains(label))
    }
    function.copy(stmts = stmts)
  }

  private def simplifyStmts(function: Function) = {
    function.transform {
      case l@LabelledStmt(assign@Assign(_, Some(True)), _) =>
        l.copy(stmt = assign.copy(constrain = None))
    }
  }
}
