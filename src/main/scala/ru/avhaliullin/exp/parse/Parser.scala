package ru.avhaliullin.exp.parse

import ru.avhaliullin.exp.ast.ASTNode
import ru.avhaliullin.exp.ast.ASTNode._

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * @author avhaliullin
  */
class Parser extends JavaTokenParsers {

  //  val literal: Parser[String] = """[a-zA-Z_][a-zA-Z_0-9]*""".r
  val literal: Parser[String] = ident

  val varName = literal

  val typeName = literal

  val fnName = literal

  val intConst: Parser[Int] = """-?[0-9]+""".r ^^ (_.toInt)

  val const = intConst ^^ ASTNode.Const

  private def binOp(arg: ~[Expression, List[~[String, Expression]]]): Expression = {
    val ~(zero, rest) = arg
    rest.foldLeft(zero) {
      (acc, it) =>
        val ~(op, rArg) = it
        Operator(acc, rArg, op)
    }
  }

  private def value: Parser[Expression] = varName ^^ Variable | const

  private def par = "(" ~> expr <~ ")"

  private def term = value | par

  // unary
  private def unary1 = term

  private val binary1Regex = """\*|/""".r

  private def binary1 = unary1 ~ rep(binary1Regex ~ unary1) ^^ binOp

  private val binary2Regex = """\+|-""".r

  private def binary2 = binary1 ~ rep(binary2Regex ~ binary1) ^^ binOp

  def expr: Parser[Expression] = binary2


  val valDefinition = "var" ~> varName ^^ (name => VarDefinition(name))

  val assignment = varName ~ "=" ~ expr ^^ {
    case varName ~ _ ~ expr =>
      Assignment(varName, expr)
  }


  val echo = "echo" ~> "(" ~> expr <~ ")" ^^ ASTNode.Echo


  val statement = (echo | valDefinition | assignment) <~ ";"

  val arg = varName ~ ":" ~ typeName ^^ {
    case varName ~ _ ~ typeName => FnDefinition.Arg(varName, typeName)
  }

  val fnSignature = "fn" ~> fnName ~ "(" ~ rep(arg) ~ ")" ~ ":" ~ typeName ^^ {
    case fnName ~ _ ~ args ~ _ ~ _ ~ retType =>
      FnDefinition.Signature(fnName, retType, args)
  }

  val fnDefinition = fnSignature ~ "=" ~ "{" ~ rep(statement) <~ "}" ^^ {
    case sig ~ _ ~ _ ~ code =>
      FnDefinition(sig, code)
  }

  val parser: Parser[List[ASTNode]] = rep(statement | fnDefinition)

  def parse(r: java.io.Reader) = parseAll(parser, r)
}
