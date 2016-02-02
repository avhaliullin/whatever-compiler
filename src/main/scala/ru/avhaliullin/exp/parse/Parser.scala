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

  private val intConst: Parser[Int] = """-?[0-9]+""".r ^^ (_.toInt)

  private val const = intConst ^^ ASTNode.IntConst

  private def binOp(arg: ~[Expression, List[~[String, Expression]]]): Expression = {
    val ~(zero, rest) = arg
    rest.foldLeft(zero) {
      (acc, it) =>
        val ~(op, rArg) = it
        BinaryOperator(acc, rArg, op)
    }
  }

  private def value: Parser[Expression] = varName ^^ Variable | const

  private def par = "(" ~> expr <~ ")"

  private def fCall = fnName ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case name ~ _ ~ args ~ _ =>
      FnCall(name, args)
  }

  private def term = ifSt | fCall | value | par

  // unary
  private def unary1 = term

  private val binary1Regex = """\*|/""".r

  private def binary1 = unary1 ~ rep(binary1Regex ~ unary1) ^^ binOp

  private val binary2Regex = """\+|-""".r

  private def binary2 = binary1 ~ rep(binary2Regex ~ binary1) ^^ binOp

  private val binary3Regex = """!=|<=|>=|<|>|==""".r

  private def binary3 = binary2 ~ rep(binary3Regex ~ binary2) ^^ binOp

  private def binary = binary3

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

  private def ifSt = "if" ~> "(" ~> expr ~")" ~ block ~ opt("else" ~> block) ^^ {
    case cond ~ _ ~ thenBlock ~ elseBlockOpt =>
      IfBlock(cond, thenBlock.exprs, elseBlockOpt.map(_.exprs).getOrElse(Seq()))
  }

  private def statement: Parser[Expression] = ((echo | valDefinition | assignment | expr) <~ ";") | block

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

  private val parser: Parser[List[ASTNode]] = rep(statement | fnDefinition)

  def parse(r: java.io.Reader) = parseAll(parser, r)
}
