package bpReduce
package transformations
package reduction

import bpReduce.ast.{Expr, Stmt, Program}
import bpReduce.ast.Stmt._
import bpReduce.ast.Stmt.Call
import bpReduce.ast.Stmt.Assume
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.Stmt.Assert

/**
 * Reduces any expression in statements.
 */
object ReduceExpr extends ProgramReducerFacory {
  def apply(program: Program): Option[ProgramReducer] = {
    val exprReducer = new StmtReducerFactory {
      def apply(stmt: Stmt) = stmt match {
        case Assign(assigns, constrain)             =>
          StmtReducer.empty(stmt) // TODO
        case assume: Assume                         =>
          AssumeExprReducer(assume)
        case Assert(e)                              =>
          StmtReducer.empty(stmt) // TODO
        case Call(name, assigns, args)              =>
          StmtReducer.empty(stmt) // TODO
        case If(condition, pos, neg)                =>
          StmtReducer.empty(stmt) // TODO
        case Return(values)                         =>
          StmtReducer.empty(stmt) // TODO
        case _: Dead | _: Goto | Skip | AtomicBegin |
             AtomicEnd | _: StartThread | EndThread =>
          StmtReducer.empty(stmt)
      }
    }
    ComposedProgramReducer(exprReducer, program)
  }
}

// TODO: generalize
final class AssumeExprReducer(assume: Assume,
                              reductions: Set[Expr]) extends StmtReducer {

  require(reductions.nonEmpty)

  def from: Stmt = assume

  def current: Option[Assume] = {
    reductions.headOption.map(assume.copy(_))
  }

  def reduce: Option[StmtReducer] = {
    reductions.headOption.flatMap(e => AssumeExprReducer(current.get, e))
  }

  def advance: Option[AssumeExprReducer] = {
    val tail: Set[Expr] = reductions.tail
    if (tail.isEmpty) None else Some(new AssumeExprReducer(assume, tail))
  }
}

object AssumeExprReducer {
  def apply(assume: Assume): Option[AssumeExprReducer] = {
    apply(assume, assume.e)
  }

  private def apply(assume: Assume, e: Expr) = {
    val reductions: Set[Expr] = ExpressionReducer(e)
    if(reductions.isEmpty) None else Some(new AssumeExprReducer(assume, reductions))
  }
}
