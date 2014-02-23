package bpReduce
package reader

import util.parsing.combinator.RegexParsers
import bpReduce.ast.Stmt._
import bpReduce.ast._
import scala.collection.mutable
import scala.util.matching.Regex
import bpReduce.ast.MixedIdentifier.{NonMixed, Mixed}
import bpReduce.ast.StateIdentifier.{Next, Current}
import bpReduce.ast.Function
import bpReduce.ast.VariableHolder
import bpReduce.ast.LabelledStmt
import bpReduce.ast.Expr._
import bpReduce.ast.Sym
import bpReduce.ast.Program

final class BooleanProgramParser extends RegexParsers {


  // TODO parens ~

  // TODO: used?
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

  def parseStmt(programStr: String): Stmt = {
    parseAll(statement, programStr) match {
      case Success(stmt, _) => stmt
      case e: NoSuccess     => sys.error("parse error: " + e.toString)
    }
  }

  def parseAssign(programStr: String): Assign = {
    parseAll(assign, programStr) match {
      case Success(stmt, _) => stmt
      case e: NoSuccess     => sys.error("parse error: " + e.toString)
    }
  }

  def parseAssume(programStr: String): Assume = {
    parseAll(assume, programStr) match {
      case Success(stmt, _) => stmt
      case e: NoSuccess     => sys.error("parse error: " + e.toString)
    }
  }

  def parseReturn(programStr: String): Return = {
    parseAll(returnStatement, programStr) match {
      case Success(stmt, _) => stmt
      case e: NoSuccess     => sys.error("parse error: " + e.toString)
    }
  }

  def parseIf(programStr: String): If = {
    parseAll(selectionStatement, programStr) match {
      case Success(stmt, _) => stmt
      case e: NoSuccess     => sys.error("parse error: " + e.toString)
    }
  }

  def parseCall(programStr: String): Call = {
    parseAll(call, programStr) match {
      case Success(stmt, _) => stmt
      case e: NoSuccess     => sys.error("parse error: " + e.toString)
    }
  }

  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

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

  lazy val function: Parser[Function] = functionHeading ~ id ~ functionParams ~ "begin" ~ decls ~ opt(enforce) ~ labelledStmtList <~ "end" ^^ {
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

  lazy val labelledStmtList: Parser[List[LabelledStmt]] = rep(labelledStmt <~ ";")

  lazy val labelledStmt: Parser[LabelledStmt] = rep(label) ~ statement ^^ {
    case labels ~ stmt => LabelledStmt(stmt, labels)
  }

  lazy val stmtList: Parser[List[Stmt]] = rep(statement <~ ";")

  lazy val statement: Parser[Stmt] = jumpStatement |
    assign |
    assertStmt |
    assume |
    selectionStatement |
    dead |
    call | // important: 'call' stmt must be checked after 'if'
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

  lazy val assign: Parser[Assign] = rep1sep(currentOrNextStateId, ",") ~ ":=" ~ assignExpr ~ opt(constrainExpr) ^^ {
    case vars ~ _ ~ exprs ~ constrain =>
      require(vars.size == exprs.size,
        s"Number of variables must be same as number of expressions: $vars, $exprs")
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

  lazy val assume: Parser[Assume] = "assume" ~> expr ^^ {
    Assume
  }

  lazy val call: Parser[Call] = opt(rep1sep(id | "_", ",") <~ ":=") ~ id ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
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

  lazy val selectionStatement: Parser[If] =
    "if" ~> expr ~ ("then" ~> labelledStmtList) ~ rep("elif" ~> expr ~ "then" ~ labelledStmtList) ~ opt("else" ~> labelledStmtList) <~ "fi" ^^ {
      case expr ~ posStmts ~ elsifs ~ elseStmtsOpt =>
        if (elsifs.isEmpty) {
          If(expr, posStmts, elseStmtsOpt.map(_.toSeq).toSeq.flatten)
        } else {
          // add last else to innermost if
          val elifs = elsifs.foldRight(elseStmtsOpt.toList.flatten) {
            case (expr ~ _ ~ stmts, elseStmts) =>
              List(LabelledStmt(If(expr, stmts, elseStmts), Seq()))
          }

          If(expr, posStmts, elifs)
        }
    }

  lazy val returnStatement: Parser[Return] = "return" ~> repsep(expr, ",") ^^ {
    Return
  }

  lazy val jumpStatement: Parser[Stmt] = returnStatement |
    "skip" ^^^ {
      Skip
    } | "goto" ~> rep1sep(id, ",") ^^ {
    Goto
  }

  lazy val expr: Parser[Expr] = xor

  lazy val xor: Parser[Expr] = rep1sep(equiv, "^" | "!=") ^^ {
    _.reduceLeft(Xor.apply)
  }

  lazy val equiv: Parser[Expr] = rep1sep(impl, "=") ^^ {
    _.reduceLeft(Equiv.apply)
  }

  lazy val impl: Parser[Expr] = rep1sep(or, "->") ^^ {
    _.reduceRight(Impl.apply)
  }

  // right-associative
  lazy val or: Parser[Expr] = rep1sep(and, "|") ^^ {
    _.reduceLeft(Or.apply)
  }

  // via right folding
  lazy val and: Parser[Expr] = rep1sep(not, "&") ^^ {
    _.reduceLeft(And.apply)
  }

  lazy val not: Parser[Expr] = opt("!") ~ atom ^^ {
    case Some(_) ~ x => Not(x)
    case _ ~ x       => x
  }

  lazy val atom: Parser[Expr] = const ^^ {
    case true  => True
    case false => False
  } | "*" ^^^ {
    Nondet
  } | currentOrNextStateId | "(" ~> expr <~ ")"

  lazy val const: Parser[Boolean] = "[Tt1]".r ^^^ true | "[Ff0]".r ^^^ false

  lazy val id: Parser[String] = """[A-Za-z][\w$]*""".r

  lazy val VarRegex: Regex = """('?)([A-Za-z]\w*)(\$?)""".r

  lazy val currentOrNextStateId: Parser[Var] = """'?[A-Za-z]\w*\$?""".r ^^ {
    case VarRegex(primed, s, mixed) =>
      val stateId = if (primed == "'") Next else Current
      val mixedId = if (mixed == "$") Mixed else NonMixed
      Var(Sym(s), stateId, mixedId)
  }

  lazy val LabelRegex = """([A-Za-z]\w*):""".r

  lazy val label: Parser[String] = """[A-Za-z]\w*:(?!=)""".r ^^ {
    case LabelRegex(label) => label
  }

  lazy val number: Parser[Int] =
    """\d+""".r ^^ {
      _.toInt
    }

}
