package bpReduce
package transformations

import bpReduce.BaseSpecification
import bpReduce.ast.Expr._
import bpReduce.ast.{Expr, Sym}
import bpReduce.ast.Expr.Var
import bpReduce.ast.Expr.Not
import bpReduce.reader.BooleanProgramParser

class ExpressionSimplifierTest extends BaseSpecification {

  "double negation" in {
    val a = Var(Sym("a"))
    val e = Not(Not(a))
    ExpressionSimplifier(e) must be_==(a)
  }

  "and" should {
    "constants" in {
      "and" in {
        val a = Var(Sym("a"))
        ExpressionSimplifier(And(a, True)) must be_==(a)
        ExpressionSimplifier(And(True, a)) must be_==(a)
        ExpressionSimplifier(And(a, False)) must be_==(False)
        ExpressionSimplifier(And(False, a)) must be_==(False)
      }

      "or" in {
        val a = Var(Sym("a"))
        ExpressionSimplifier(Or(a, True)) must be_==(True)
        ExpressionSimplifier(Or(True, a)) must be_==(True)
        ExpressionSimplifier(Or(a, False)) must be_==(a)
        ExpressionSimplifier(Or(False, a)) must be_==(a)
      }
    }

    "duplicated ops" in {
      val a = Var(Sym("a"))
      val b = Var(Sym("b"))
      val c = Var(Sym("c"))
      ExpressionSimplifier(And(a, b, c, a, b, c)) must be_==(And(a, b, c))
    }

    "flattening" in {
      val a = Var(Sym("a"))
      val b = Var(Sym("b"))
      val c = Var(Sym("c"))
      ExpressionSimplifier(And(And(a, b), c)) must be_==(And(a, b, c))
    }

    "=" in {
      ExpressionSimplifier(Equiv(True, False)) === False
    }

    "nondet" in {
      ExpressionSimplifier(Equiv(Nondet, Nondet)) === Nondet
    }

    "constants" in {
      ExpressionSimplifier(Not(False)) === True
      ExpressionSimplifier(Not(True)) === False
    }
  }

  "from parser" should {
    implicit def fromText(expr: String) = {
      new BooleanProgramParser().parseExpr(expr)
    }

    "!(T & F & !(F) | T & !(T) & !(F) | F)" in {
      val expr: Expr = "!(T & F & !(F) | T & !(T) & !(F) | F)"
      ExpressionSimplifier(expr) === True
    }
  }
}
