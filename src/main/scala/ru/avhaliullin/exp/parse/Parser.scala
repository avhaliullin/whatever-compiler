package ru.avhaliullin.exp.parse

import ru.avhaliullin.exp.ast.ASTNode
import ru.avhaliullin.exp.ast.ASTNode._

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * @author avhaliullin
  */
class Parser extends JavaTokenParsers {

  //  val literal: Parser[String] = """[a-zA-Z_][a-zA-Z_0-9]*""".r
  private val literal: Parser[String] = ident

  private val varName = literal

  private val typeName = literal

  private val fnName = literal

  private val intConst = ("""-?[0-9]+""".r ^^ (_.toInt)) ^^ IntConst

  private val boolConst = ("true" ^^ (_ => true) | "false" ^^ (_ => false)) ^^ BoolConst

  private val const = intConst | boolConst

  private def binOp(arg: ~[Expression, List[~[String, Expression]]]): Expression = {
    val ~(zero, rest) = arg
    rest.foldLeft(zero) {
      (acc, it) =>
        val ~(op, rArg) = it
        BinaryOperator(acc, rArg, op)
    }
  }

  private def value: Parser[Expression] = const | varName ^^ Variable

  private def par = "(" ~> expr <~ ")"

  private def fCall = fnName ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case name ~ _ ~ args ~ _ =>
      FnCall(name, args)
  }

  private def term = ifSt | fCall | value | par | block

  // unary
  private val unaryRegex =
    """-|!""".r

  private def unary = rep(unaryRegex) ~ term ^^ {
    case ops ~ expr => ops.foldRight(expr: Expression) {
      (op, e) =>
        UnaryOperator(e, op)
    }
  }

  private val binary1Regex = """\*|/""".r

  private def binary1 = unary ~ rep(binary1Regex ~ unary) ^^ binOp

  private val binary2Regex = """\+|-""".r

  private def binary2 = binary1 ~ rep(binary2Regex ~ binary1) ^^ binOp

  private val binary3Regex = """<=|>=|<|>""".r

  private def binary3 = binary2 ~ rep(binary3Regex ~ binary2) ^^ binOp

  private val binary4Regex = """!=|==""".r

  private def binary4 = binary3 ~ rep(binary4Regex ~ binary3) ^^ binOp

  private val binary5Regex = "&"

  private def binary5 = binary4 ~ rep(binary5Regex ~ binary4) ^^ binOp

  private val binary6Regex = "^"

  private def binary6 = binary5 ~ rep(binary6Regex ~ binary5) ^^ binOp

  private val binary7Regex = "|"

  private def binary7 = binary6 ~ rep(binary7Regex ~ binary6) ^^ binOp

  private def binary = binary7

  private def expr: Parser[Expression] = binary


  private val valDefinition = "var" ~> varName ~ ":" ~ typeName ^^ {
    case name ~ _ ~ tpe => VarDefinition(name, tpe)
  }

  private val assignment = varName ~ "=" ~ expr ^^ {
    case varName ~ _ ~ expr =>
      Assignment(varName, expr)
  }


  private val echo = "echo" ~> "(" ~> expr <~ ")" ^^ ASTNode.Echo

  private def block: Parser[Block] = "{" ~> rep(statement) <~ "}" ^^ Block

  private def ifSt = "if" ~> "(" ~> expr ~ ")" ~ block ~ opt("else" ~> block) ^^ {
    case cond ~ _ ~ thenBlock ~ elseBlockOpt =>
      IfBlock(cond, thenBlock.exprs, elseBlockOpt.map(_.exprs).getOrElse(Seq()))
  }

  private def statement: Parser[Expression] = echo | valDefinition | assignment | expr

  private val arg = varName ~ ":" ~ typeName ^^ {
    case varName ~ _ ~ typeName => FnDefinition.Arg(varName, typeName)
  }

  private val fnSignature = "fn" ~> fnName ~ "(" ~ repsep(arg, ",") ~ ")" ~ ":" ~ typeName ^^ {
    case fnName ~ _ ~ args ~ _ ~ _ ~ retType =>
      FnDefinition.Signature(fnName, retType, args)
  }

  private val fnDefinition = fnSignature ~ "=" ~ "{" ~ rep(statement) <~ "}" ^^ {
    case sig ~ _ ~ _ ~ code =>
      FnDefinition(sig, code)
  }

  private val parser: Parser[List[ASTNode]] = rep(fnDefinition | statement)

  def parse(r: java.io.Reader) = parseAll(parser, r)
}
