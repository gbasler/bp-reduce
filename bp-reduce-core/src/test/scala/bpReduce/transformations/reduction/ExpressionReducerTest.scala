package bpReduce
package transformations
package reduction

import bpReduce.BaseSpecification
import bpReduce.ast.Expr._
import bpReduce.ast.Sym
import bpReduce.ast.Expr.Var

class ExpressionReducerTest extends BaseSpecification {

  "reduce expressions" should {

    "no reduction" in {
      ExpressionReducer(True) === Set()
      ExpressionReducer(False) === Set()
    }

    "one reduction" in {
      val a = Var(Sym("a"))
      val b = Var(Sym("b"))

      ExpressionReducer(And(a, b)) === Set(a, b, False)
      ExpressionReducer(Or(a, b)) === Set(a, b, True)
    }

    "two reductions" in {
      val a = Var(Sym("a"))
      val b = Var(Sym("b"))
      val c = Var(Sym("c"))

      ExpressionReducer(And(a, Or(b, c))) === Set(And(a, b), And(a, c), Or(b, c), a, False)
    }

    "nondet" in {
      ExpressionReducer(Nondet) === Set(True, False)
      ExpressionReducer(Or(Nondet, Nondet)) === Set(True, False)
      ExpressionReducer(Equiv(Nondet, Nondet)) === Set(True, False)
    }
  }

}
