package bpReduce.ast

sealed abstract class Stmt

object Stmt {
  final case class Assign extends Stmt
  final case class Assume extends Stmt
  final case class Assert extends Stmt
  final case class Goto extends Stmt
  final case class If extends Stmt
  final case class Skip extends Stmt
  final case class Return extends Stmt
  final case class AtomicBegin extends Stmt
  final case class AtomicEnd extends Stmt
  final case class StartThread extends Stmt
  final case class EndThread extends Stmt

}

