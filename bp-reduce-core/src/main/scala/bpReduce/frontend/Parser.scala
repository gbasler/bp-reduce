package bpReduce.frontend

import util.parsing.combinator.RegexParsers
import bpReduce.ast.Stmt._
import bpReduce.ast.Expr._
import scala.util.matching.Regex
import bpReduce.ast.Expr

final class Parser extends RegexParsers {

  def program = (globalDecl *) ~ (function *)

  def globalDecl = "decl" ~ id ~ rep("," ~ id) ~ ";"

  def function = ""

  def expr = xor

  def xor: Parser[Expr] = rep1sep(equiv, "^") ^^ {
    _.reduceLeft(Xor)
  }

  def equiv: Parser[Expr] = rep1sep(impl, "<->") ^^ {
    _.reduceLeft(Equiv)
  }

  def impl: Parser[Expr] = rep1sep(or, "->") ^^ {
    _.reduceRight(Impl)
  }

  // right-associative
  def or: Parser[Expr] = rep1sep(and, "|") ^^ {
    _.reduceLeft(Or)
  }

  // via right folding
  def and: Parser[Expr] = rep1sep(not, "&") ^^ {
    _.reduceLeft(And)
  }

  def not: Parser[Expr] = opt("!") ~ atom ^^ {
    case Some(_) ~ x => Not(x)
    case _ ~ x => x
  }

  def atom: Parser[Expr] = (const ^^ Const
    | id ^^ Id
    | "(" ~> expr <~ ")"
    | "[" ~> expr <~ "]"
    )

  def const: Parser[String] = "T" | "F"

  def id: Regex = """[a-z]\w*""".r
}
