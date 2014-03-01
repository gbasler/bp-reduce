package bpReduce
package reduction

import bpReduce.BaseSpecification
import bpReduce.ast.Expr._
import bpReduce.ast.Sym
import bpReduce.ast.Expr.Var

class ExpressionReducerTest extends BaseSpecification {

  "reduce expressions" should {

    "no reduction" in {
      ExpressionReducer(True) must beEmpty
      ExpressionReducer(False) must beEmpty
    }

    "one reduction" in {
      val a = Var(Sym("a"))
      val b = Var(Sym("b"))

      ExpressionReducer(And(a, b)) === List(b, False, a)
      ExpressionReducer(Or(a, b)) === List(True, b, a)
    }

    "two reductions" in {
      val a = Var(Sym("a"))
      val b = Var(Sym("b"))
      val c = Var(Sym("c"))

      ExpressionReducer(And(a, Or(b, c))) === List(Or(b, c), False, a, And(a, c), And(a, b))
    }

    "nondet" in {
      ExpressionReducer(Nondet) === List(True, False)
      ExpressionReducer(Or(Nondet, Nondet)) === List(True, False)
      ExpressionReducer(Equiv(Nondet, Nondet)) === List(True, False)
    }
  }

}
