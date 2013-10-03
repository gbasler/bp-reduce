package bpReduce.ast

import scala.collection.mutable

sealed abstract class Expr

object Expr {

  final case class And(a: Expr, b: Expr) extends Expr

  final case class Or(a: Expr, b: Expr) extends Expr

  final case class Impl(a: Expr, b: Expr) extends Expr

  final case class Xor(a: Expr, b: Expr) extends Expr

  final case class Equiv(a: Expr, b: Expr) extends Expr

  final case class Schoose(a: Expr, b: Expr) extends Expr

  final case class Not(a: Expr) extends Expr

  final case class Const(name: String) extends Expr {
    override def toString = name
  }

  final case class Id(name: String) extends Expr {
    varNames += name;

    override def toString = name
  }

  var varNames = mutable.Set[String]()
}