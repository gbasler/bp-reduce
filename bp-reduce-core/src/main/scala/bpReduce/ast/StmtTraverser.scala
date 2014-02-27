package bpReduce
package ast

import bpReduce.ast.Stmt._
import bpReduce.ast.Stmt.Call
import bpReduce.ast.Stmt.Assume
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.Stmt.Assert

class StmtTraverser {

  def traverse(stmt: LabelledStmt): Unit = stmt.stmt match {
    case Assign(assigns, constrain) =>
    case Assume(e)                  =>
    case Assert(e)                  =>
    case Call(name, assigns, args)  =>
    case Dead(vars)                 =>
    case Goto(targets)              =>
    case If(condition, pos, neg)    =>
      traverseTrees(pos)
      traverseTrees(neg)
    case Skip                       =>
    case Return(values)             =>
    case AtomicBegin                =>
    case AtomicEnd                  =>
    case StartThread(label)         =>
    case EndThread                  =>
  }

  /** Traverses a list of stmts. */
  def traverseTrees(trees: List[LabelledStmt]) {
    trees foreach traverse
  }
}
