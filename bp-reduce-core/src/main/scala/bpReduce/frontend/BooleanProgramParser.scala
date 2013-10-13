package bpReduce.frontend

import util.parsing.combinator.RegexParsers
import bpReduce.ast.Stmt._
import bpReduce.ast._
import scala.collection.mutable
import bpReduce.ast.Expr.Or
import bpReduce.ast.Expr.Equiv
import bpReduce.ast.Expr.Id
import bpReduce.ast.Stmt.Dead
import bpReduce.ast.Expr.Schoose
import bpReduce.ast.Expr.Not
import bpReduce.ast.Function
import bpReduce.ast.Sym
import bpReduce.ast.Stmt.Assert
import bpReduce.ast.Expr.Impl
import bpReduce.ast.Stmt.Call
import bpReduce.ast.Expr.And
import bpReduce.ast.Stmt.Assume
import bpReduce.ast.Expr.Xor
import bpReduce.ast.Stmt.StartThread
import bpReduce.ast.Stmt.Goto
import bpReduce.ast.Stmt.Return


final class BooleanProgramParser extends RegexParsers {

  // TODO parens ~

  // TODO: labels: add to abstrct base class
  // parser returns factory instead of stmt
  // factory creates stmt, adds labels and fixes symbols

  object Scope {

    private val scopes = mutable.Stack[mutable.Set[Sym]]()

    openScope()

    def openScope() {
      scopes.push(mutable.Set())
    }

    def closeScope() {
      scopes.pop()
    }

    def contains(sym: Sym): Boolean = {
      scopes.exists(_.contains(sym))
    }

    def +=(sym: Sym) = {
      scopes.top += sym
    }
  }

  def parse(programStr: String): Program = {
    parseAll(program, programStr) match {
      case Success(program, _) => program
      case e: NoSuccess        => sys.error("parse error: " + e.toString)
    }
  }

  lazy val program: Parser[Program] = decls ~ rep(function) ^^ {
    case globals ~ functions => Program(globals, functions)
  }

  lazy val decls: Parser[VariableHolder] = rep(decl) ^^ {
    listOfDecls => VariableHolder(listOfDecls.flatten)
  }

  lazy val decl: Parser[List[Sym]] = "decl" ~> rep1sep(id, ",") <~ ";" ^^ {
    list =>
      val vars = list.map {
        name =>
          val sym = Sym(name)
          if (Scope.contains(sym)) {
            failure(s"$sym already defined")
          } else {
            Scope += sym
          }
          sym
      }
      vars
  }

  lazy val function: Parser[Function] = functionHeading ~ id ~ functionParams ~ "begin" ~ decls ~ opt(enforce) ~ statementList <~ "end" ^^ {
    case heading ~ name ~ params ~ _ ~ vars ~ _ ~ stmts => Function(name, vars, params, heading, stmts)
  }

  lazy val functionParams: Parser[List[String]] = "(" ~> repsep(id, ",") <~ ")"

  lazy val functionHeading: Parser[Int] = opt("dfs") ~> functionType

  lazy val functionType: Parser[Int] = "void" ^^^ {
    0
  } | "bool" ~> opt("<" ~> number <~ ">") ^^ {
    case Some(x) => x
    case None    => 1
  }

  lazy val enforce: Parser[Expr] = "enforce" ~> expr <~ ";"

  lazy val statementList: Parser[List[Stmt]] = rep(labelledStmt <~ ";")

  lazy val labelledStmt: Parser[Stmt] = rep(label) ~ concurrentStatement  ^^ {
    case labels ~ stmt => /*labels ->*/ stmt
  }

  lazy val concurrentStatement: Parser[Stmt] = statement | startThread | endThread | atomicBegin | atomicEnd |
    failure("statement expected")

  lazy val startThread: Parser[Stmt] = "start_thread" ~> "goto" ~> id ^^ {
    StartThread
  }

  lazy val endThread: Parser[Stmt] = "end_thread" ^^^ EndThread

  lazy val atomicBegin: Parser[Stmt] = "atomic_begin" ^^^ AtomicBegin

  lazy val atomicEnd: Parser[Stmt] = "atomic_end" ^^^ AtomicEnd

  lazy val statement: Parser[Stmt] = dead | assign | assertStmt | assume | call | selection_statement | jump_statement

  lazy val dead: Parser[Stmt] = "dead" ~> rep1sep(id, ",") ^^ {
    Dead
  }

  lazy val assign: Parser[Stmt] = parallelAssign | callAssign

  lazy val parallelAssign: Parser[Stmt] = rep1sep(id, ",") ~ ":=" ~ assignExpr ~ opt(constrainExpr) ^^ {
    case vars ~ _ ~ exprs ~ Some(constrain) => Skip //Assign
    case vars ~ _ ~ exprs ~ None            =>
      require(vars.size == exprs.size, "Number of variables must be same as number of expressions")
      val syms = vars.map(Sym(_))
      Assign(syms.zip(exprs))
  }

  lazy val callAssign: Parser[Stmt] = rep1sep(assignId, ",") ~ ":=" ~ "call" <~ id ^^ {
    case vars ~ _ ~ call => Skip //Assign
  }

  lazy val assignId: Parser[Option[String]] = id ^^ {
    Some(_)
  } | "_" ^^^ {
    None
  }

  lazy val assignExpr: Parser[List[Expr]] = rep1sep(schooseExpr | expr, ",")

  lazy val schooseExpr: Parser[Expr] = "schoose" ~ "[" ~ expr ~ "," ~ expr ~ "]" ^^ {
    case _ ~ _ ~ pos ~ _ ~ neg ~ _ => Schoose(pos, neg)
  }

  lazy val constrainExpr: Parser[Expr] = "constrain" ~> expr

  lazy val assertStmt: Parser[Stmt] = "assert" ~> expr ^^ {
    Assert
  }

  lazy val assume: Parser[Stmt] = "assume" ~> expr ^^ {
    Assume
  }

  lazy val call: Parser[Stmt] = opt(repsep(id, ",") <~ ":=") ~ id ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case vars ~ id ~ _ ~ args ~ _ => Skip // Call(id, args)
  }

  lazy val selection_statement: Parser[Stmt] =
    "if" ~> expr ~ ("then" ~> statementList) ~ rep("elif" ~> expr <~ "then" ~ statementList) ~ opt("else" ~> statementList) <~ "fi" ^^ {
      case expr ~ posStmts ~ elsifs ~ Some(stmts) => Skip
    }
  //  lazy val selection_statement: Parser[Stmt] =
  //    "if" ~> expr ~ ("then" ~> labelledStmt) ~ ("else" ~> labelledStmt) ~ "fi" ^^^ Skip |
  //    "if" ~> expr ~ ("then" ~> labelledStmt) ~ "fi" ^^^ Skip

  lazy val jump_statement: Parser[Stmt] = "return" ~> repsep(expr, ",") ^^ {
    Return
  } | "skip" ^^^ {
    Skip
  } | "goto" ~> rep1sep(id, ",") ^^ {
    Goto
  }

  lazy val expr: Parser[Expr] = xor

  lazy val xor: Parser[Expr] = rep1sep(equiv, "^") ^^ {
    _.reduceLeft(Xor)
  }

  lazy val equiv: Parser[Expr] = rep1sep(impl, "<->") ^^ {
    _.reduceLeft(Equiv)
  }

  lazy val impl: Parser[Expr] = rep1sep(or, "->") ^^ {
    _.reduceRight(Impl)
  }

  // right-associative
  lazy val or: Parser[Expr] = rep1sep(and, "|") ^^ {
    _.reduceLeft(Or)
  }

  // via right folding
  lazy val and: Parser[Expr] = rep1sep(not, "&") ^^ {
    _.reduceLeft(And)
  }

  lazy val not: Parser[Expr] = opt("!") ~ atom ^^ {
    case Some(_) ~ x => Not(x)
    case _ ~ x       => x
  }

  lazy val atom: Parser[Expr] = const ^^ {
    case true  => Expr.True
    case false => Expr.False
  } | id ^^ Id | "(" ~> expr <~ ")"

  lazy val const: Parser[Boolean] = "[Tt1]".r ^^^ true | "[Ff0]".r ^^^ false

  lazy val id: Parser[String] = """[A-Za-z]\w*""".r

  lazy val label: Parser[String] = """[A-Za-z]\w*:""".r

  lazy val number: Parser[Int] =
    """(\d+)""".r ^^ {
      _.toInt
    }

}
