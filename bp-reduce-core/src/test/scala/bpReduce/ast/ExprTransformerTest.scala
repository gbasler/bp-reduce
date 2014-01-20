package bpReduce
package ast

import bpReduce.ast.Expr._
import bpReduce.ast.Expr.Var


class ExprTransformerTest extends BaseSpecification {

  "transform" should {
    "swap and <-> or" in {
      val a = Var(Sym("a"))
      val b = Var(Sym("b"))
      val c = Var(Sym("c"))
      val e = Var(Sym("c"))
      val f = Var(Sym("c"))
      val expr = And(a, b, c, a, b, Nondet, Or(e, f))
      val actual = expr.transform {
        case NaryOp(And, ops) => NaryOp(Or, ops)
        case NaryOp(Or, ops)  => NaryOp(And, ops)
      }
      actual must be_==(Or(a, b, c, a, b, Nondet, And(e, f)))
    }
  }
}
