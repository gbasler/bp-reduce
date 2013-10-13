package bpReduce.ast

import scala.collection.mutable

sealed abstract class Expr

object Expr {

  final case class And(a: Expr, b: Expr) extends Expr

  final case class Or(a: Expr, b: Expr) extends Expr

  final case class Impl(a: Expr, b: Expr) extends Expr

  final case class Xor(a: Expr, b: Expr) extends Expr

  final case class Equiv(a: Expr, b: Expr) extends Expr

  final case class Schoose(pos: Expr, neg: Expr) extends Expr

  final case class Not(a: Expr) extends Expr

  case object True extends Expr

  case object False extends Expr

  final case class Id(name: String) extends Expr {
    varNames += name;

    override def toString = name
  }

  var varNames = mutable.Set[String]()
}