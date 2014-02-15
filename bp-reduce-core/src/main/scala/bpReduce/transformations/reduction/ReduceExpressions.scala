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
object ReduceExpressions extends ProgramReducerFacory {
  def apply(program: Program): Option[ProgramReducer] = {
    val exprReducer = new StmtReducerFactory {
      override def apply(stmt: Stmt): StmtReducer = stmt match {
        case Assign(assigns, constrain)             =>
          ???
        case assume: Assume                         =>
          AssumeReducer(assume)
        case Assert(e)                              =>
          ???
        case Call(name, assigns, args)              =>
          ???
        case If(condition, pos, neg)                =>
          ???
        case Return(values)                         =>
          ???
        case _: Dead | _: Goto | Skip | AtomicBegin |
             AtomicEnd | _: StartThread | EndThread =>
          StmtReducer.Empty
      }
    }
    ComposedProgramReducer(exprReducer, program)
  }
}

final class AssumeReducer(assume: Assume,
                          reductions: Set[Expr]) extends StmtReducer {

  override def current: Option[Stmt] = {
    reductions.headOption.map(assume.copy(_))
  }

  override def reduce: Option[StmtReducer] = {
    val tail: Set[Expr] = reductions.tail
    if (tail.isEmpty) None else Some(new AssumeReducer(assume, tail))
  }

  override def advance: Option[StmtReducer] = {
    reductions.headOption.map(e => AssumeReducer(assume, e))
  }
}

object AssumeReducer {
  def apply(assume: Assume): AssumeReducer = {
    apply(assume, assume.e)
  }

  private def apply(assume: Assume, e: Expr) = {
    val reductions = ExpressionReducer(e)
    new AssumeReducer(assume, reductions)
  }
}