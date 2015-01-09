package bpReduce
package ast

import bpReduce.ast.Stmt._
import bpReduce.ast.Stmt.Call
import bpReduce.ast.Stmt.Assume
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.Stmt.Assert

class StmtTraverser {

  def traverse(stmt: Stmt): Unit = stmt match {
    case Assign(assigns, constrain) =>
    case Assume(e)                  =>
    case Assert(e)                  =>
    case Call(name, assigns, args)  =>
    case Dead(vars)                 =>
    case Goto(targets)              =>
    case If(condition, pos, neg)    =>
      traverseTrees(pos.map(_.stmt))
      traverseTrees(neg.map(_.stmt))
    case Skip                       =>
    case Return(values)             =>
    case AtomicBegin                =>
    case AtomicEnd                  =>
    case StartThread(label)         =>
    case EndThread                  =>
  }

  /** Traverses a list of stmts. */
  def traverseTrees(trees: List[Stmt]) {
    trees foreach traverse
  }
}

final class StmtTraverserForExpr[T](pf: PartialFunction[Expr, T]) extends StmtTraverser {
  override def traverse(stmt: Stmt) = stmt match {
    case Assign(assigns, constrain) =>
      assigns.map(_._2.collect(pf))
      constrain.map(_.collect(pf))
    case Assume(e)                  =>
      e.collect(pf)
    case Assert(e)                  =>
      e.collect(pf)
    case Call(name, assigns, args)  =>
      args.map(_.collect(pf))
    case If(condition, pos, neg)    =>
      condition.collect(pf)
      traverseTrees(pos.map(_.stmt))
      traverseTrees(neg.map(_.stmt))
    case Return(values)             =>
      values.map(_.collect(pf))
    case _                          =>
      super.traverse(stmt)
  }
}
