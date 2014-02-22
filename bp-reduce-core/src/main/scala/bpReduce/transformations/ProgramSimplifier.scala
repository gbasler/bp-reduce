package bpReduce
package transformations

import bpReduce.ast.{LabelledStmt, Stmt, Program, Function}

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
    ???
  }

  private def buildBlocks(function: Function): List[Block] = {
    val blocks = for {
      stmt <- function.stmts
    } yield {
      Block(List(stmt.stmt), stmt.labels)
    }
    blocks
  }

  private def labelledStmtsForBlocks(blocks: List[Block]) = {
    for {
      block <- blocks
    } yield {
      LabelledStmt(block.stmt.head, block.labels)
    }
  }

  //    private def removeSkips

}
