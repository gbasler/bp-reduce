package bpReduce
package ast

import bpReduce.ast.Expr.{Or, Nondet, And, Var}


class ExprTransformerTest extends BaseSpecification {

  "transform" in {
    val a = Var(Sym("a"))
    val b = Var(Sym("b"))
    val c = Var(Sym("c"))
    val e = Var(Sym("c"))
    val f = Var(Sym("c"))
    val expr = And(a, b, c, a, b, Nondet, Or(e, f))
    val actual = expr.tr {
      case v: Var => v
    }
    actual must be_==(Seq(a, b, c, a, b, e, f))

  }
}
