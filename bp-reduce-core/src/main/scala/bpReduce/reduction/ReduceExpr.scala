package bpReduce
package reduction

import bpReduce.ast.{Stmt, Program}
import bpReduce.ast.Stmt._
import bpReduce.ast.Stmt.Call
import bpReduce.ast.Stmt.Assume
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.Stmt.Assert

/**
 * Reduces any expression in statements.
 */
case object ReduceExpr extends ProgramReducerFactory {
  def apply(program: Program): Option[ProgramReducer] = {
    val exprReducer = new StmtReducerFactory {
      def apply(stmt: Stmt) = stmt match {
        case assign: Assign                         =>
          ReduceAssignExpr(assign)
        case assume: Assume                         =>
          ReduceAssumeExpr(assume)
        case Assert(e)                              =>
          None // reducing assert doesn't make much sense...
        case call: Call                             =>
          ReduceCallExpr(call)
        case iff: If                                =>
          ReduceIfExpr(iff)
        case ret: Return                            =>
          ReduceReturnExpr(ret)
        case _: Dead | _: Goto | Skip | AtomicBegin |
             AtomicEnd | _: StartThread | EndThread =>
          None
      }
    }
    ComposedProgramReducer(exprReducer, program)
  }
}


//
//final class GeneralReducer[T <: Stmt](override val from: T,
//                                      reductions: List[Expr])
//                                     ((T)
//                                     (reducerFactory: (T, List[Expr]) => StmtReducer) extends StmtReducer {
//  require(reductions.nonEmpty)
//
//  val to: T = from.copy(reductions.head)
//
//  def reduce: Option[StmtReducer] = {
//    ExpressionReducer(reductions.head).toList match {
//      case Nil        => None
//      case reductions => Some(new AssumeExprReducer(assume, reductions))
//    }
//    AssumeExprReducer(to, reductions.head)
//  }
//
//  def advance: Option[AssumeExprReducer] = reductions match {
//    case _ :: Nil     => None
//    case head :: tail => Some(new GeneralReducer(from, tail))
//  }
//}
//
//object GeneralReducer {
//  def apply(assume: Assume): Option[AssumeExprReducer] = {
//    apply(assume, assume.e)
//  }
//
//  private def apply(assume: Assume, e: Expr) = {
//    ExpressionReducer(e).toList match {
//      case Nil        => None
//      case reductions => Some(new AssumeExprReducer(assume, reductions))
//    }
//  }
//}