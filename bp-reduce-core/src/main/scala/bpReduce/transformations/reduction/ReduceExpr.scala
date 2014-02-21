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
          None // TODO
        case assume: Assume                         =>
          AssumeExprReducer(assume)
        case Assert(e)                              =>
          None // TODO
        case Call(name, assigns, args)              =>
          None // TODO
        case If(condition, pos, neg)                =>
          None // TODO
        case Return(values)                         =>
          None // TODO
        case _: Dead | _: Goto | Skip | AtomicBegin |
             AtomicEnd | _: StartThread | EndThread =>
          None
      }
    }
    ComposedProgramReducer(exprReducer, program)
  }
}

// TODO: generalize
final class AssumeExprReducer(override val from: Assume,
                              reductions: List[Expr]) extends StmtReducer {

  require(reductions.nonEmpty)

  val to: Assume = from.copy(reductions.head)

  def reduce: Option[StmtReducer] = AssumeExprReducer(to, reductions.head)

  def advance: Option[AssumeExprReducer] = reductions match {
    case _ :: Nil     => None
    case head :: tail => Some(new AssumeExprReducer(from, tail))
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
