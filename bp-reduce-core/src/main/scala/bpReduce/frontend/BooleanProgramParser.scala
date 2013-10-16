package bpReduce.frontend

import util.parsing.combinator.RegexParsers
import bpReduce.ast.Stmt._
import bpReduce.ast._
import scala.collection.mutable
import bpReduce.ast.Stmt.Dead
import bpReduce.ast.Function
import bpReduce.ast.Sym
import bpReduce.ast.Stmt.Assert
import bpReduce.ast.Stmt.Assume
import bpReduce.ast.Stmt.StartThread
import bpReduce.ast.Stmt.Goto
import bpReduce.ast.Stmt.Return
import scala.util.matching.Regex

final class BooleanProgramParser extends RegexParsers {

  import Expr._

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
      case e: NoSuccess => sys.error("parse error: " + e.toString)
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
    case None => 1
  }

  lazy val enforce: Parser[Expr] = "enforce" ~> expr <~ ";"

  lazy val statementList: Parser[List[Stmt]] = rep(labelledStmt <~ ";")

  lazy val labelledStmt: Parser[Stmt] = rep(label) ~ statement ^^ {
    case labels ~ stmt => /*labels ->*/ stmt
  }

  lazy val statement: Parser[Stmt] = dead |
    assign |
    assertStmt |
    assume |
    call |
    selectionStatement |
    jumpStatement |
    startThread |
    endThread |
    atomicBegin |
    atomicEnd |
    failure("statement expected")

  lazy val startThread: Parser[Stmt] = "start_thread" ~> "goto" ~> id ^^ {
    StartThread
  }

  lazy val endThread: Parser[Stmt] = "end_thread" ^^^ EndThread

  lazy val atomicBegin: Parser[Stmt] = "atomic_begin" ^^^ AtomicBegin

  lazy val atomicEnd: Parser[Stmt] = "atomic_end" ^^^ AtomicEnd

  lazy val dead: Parser[Stmt] = "dead" ~> rep1sep(id, ",") ^^ {
    case vars => Dead(vars.map(Sym(_)))
  }

  lazy val assign: Parser[Stmt] = rep1sep(currentOrNextStateId, ",") ~ ":=" ~ assignExpr ~ opt(constrainExpr) ^^ {
    case vars ~ _ ~ exprs ~ constrain =>
      require(vars.size == exprs.size, "Number of variables must be same as number of expressions")
      // in fact, vars always refer to next state...
      Assign(vars.zip(exprs), constrain)
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

  lazy val call: Parser[Stmt] = opt(rep1sep(id | "_", ",") <~ ":=") ~ id ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case vars ~ id ~ _ ~ args ~ _ =>
      val varsList = for {
        vs <- vars.toList
        v <- vs
      } yield {
        if (v == "_") {
          None
        } else {
          Some(Sym(v))
        }
      }

      Call(id, varsList, args)
  }

  lazy val selectionStatement: Parser[Stmt] =
    "if" ~> expr ~ ("then" ~> statementList) ~ rep("elif" ~> expr ~ "then" ~ statementList) ~ opt("else" ~> statementList) <~ "fi" ^^ {
      case expr ~ posStmts ~ elsifs ~ elseStmtsOpt =>
        if (elsifs.isEmpty) {
          If(expr, posStmts, elseStmtsOpt.map(_.toSeq).toSeq.flatten)
        } else {
          // add last else to innermost if
          val elifs = elsifs.foldRight(elseStmtsOpt.toList.flatten) {
            case (expr ~ _ ~ stmts, elseStmts) =>
              List(If(expr, stmts, elseStmts))
          }

          If(expr, posStmts, elifs)
        }
    }

  lazy val jumpStatement: Parser[Stmt] = "return" ~> repsep(expr, ",") ^^ {
    Return
  } | "skip" ^^^ {
    Skip
  } | "goto" ~> rep1sep(id, ",") ^^ {
    Goto
  }

  lazy val expr: Parser[Expr] = xor

  lazy val xor: Parser[Expr] = rep1sep(equiv, "^" | "!=") ^^ {
    _.reduceLeft(Xor)
  }

  lazy val equiv: Parser[Expr] = rep1sep(impl, "=") ^^ {
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
    case _ ~ x => x
  }

  lazy val atom: Parser[Expr] = const ^^ {
    case true => True
    case false => False
  } | "*" ^^^ {
    Nondet
  } | currentOrNextStateId | "(" ~> expr <~ ")"

  lazy val const: Parser[Boolean] = "[Tt1]".r ^^^ true | "[Ff0]".r ^^^ false

  lazy val id: Parser[String] = """$?[A-Za-z]\w*""".r

  lazy val VarRegex: Regex = """('?)(\$?)(.*)""".r

  lazy val currentOrNextStateId: Parser[Var] = """'?\$?[A-Za-z]\w*""".r ^^ {
    case VarRegex(primed, mixed, s) =>
      import StateIdentifier._
      import MixedIdentifier._
      val stateId = if(primed == "'") Next else Current
      val mixedId = if(mixed == "$") Mixed else NonMixed
      Var(Sym(s), stateId, mixedId)
  }

  lazy val label: Parser[String] = """[A-Za-z]\w*:""".r

  lazy val number: Parser[Int] =
    """\d+""".r ^^ {
      _.toInt
    }

}
