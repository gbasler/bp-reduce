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
                              reductions: List[Expr]) extends StmtReducer {

  require(reductions.nonEmpty)

  def from: Stmt = assume

  val to: Assume = assume.copy(reductions.head)

  def reduce: Option[StmtReducer] = AssumeExprReducer(to, reductions.head)

  def advance: Option[AssumeExprReducer] = reductions match {
    case _ :: Nil     => None
    case head :: tail => Some(new AssumeExprReducer(assume, tail))
  }
}

object AssumeExprReducer {
  def apply(assume: Assume): Option[AssumeExprReducer] = {
    apply(assume, assume.e)
  }

  private def apply(assume: Assume, e: Expr) = {
    ExpressionReducer(e).toList match {
      case Nil        => None
      case reductions => Some(new AssumeExprReducer(assume, reductions))
    }
  }
}
