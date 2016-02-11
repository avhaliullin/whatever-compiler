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

  private def argList = "(" ~>
    repsep((literal <~ ":=").? ~ expr ^^ {
      case Some(name) ~ value => ASTNode.Argument.ByName(name, value)
      case None ~ value => ASTNode.Argument.ByOrder(value)
    }, ",") <~ ")"

  private def fCall = fnName ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case name ~ args =>
      FnCall(name, args)
  }

  private def sInstantiation = "new" ~> typeName ~ argList ^^ {
    case tName ~ args =>
      ASTNode.StructInstantiation(tName, args)
  }


  private def term = (sInstantiation | ifSt | fCall | value | par | block) ~ ("." ~> rep1sep(varName, ".")).? ^^ {
    case e ~ Some(fields) =>

      fields.foldLeft(e) {
        (acc, it) =>
          ASTNode.FieldAccess(it, acc)
      }
    case e ~ None => e
  }

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

  private val binary8Regex = "&&"

  private def binary8 = binary7 ~ rep(binary8Regex ~ binary7) ^^ binOp

  private val binary9Regex = "||"

  private def binary9 = binary8 ~ rep(binary9Regex ~ binary8) ^^ binOp

  private val assignment = binary9 ~ rep("=" ~> binary9) ^^ {
    case assignee ~ value =>
      (assignee :: value).reduceRight {
        (assignee, value) =>
          ASTNode.Assignment(assignee, value)
      }
  }

  private def binary = assignment


  private def expr: Parser[Expression] = binary


  private val varDefinition = "var" ~> varName ~ (":" ~> typeName).? ~ ("=" ~> expr).? ^^ {
    case name ~ None ~ None => throw new RuntimeException(s"Variable $name doesn't have type nor assigned")
    case name ~ Some(tpe) ~ None => VarDefinition(name, tpe)
    case name ~ tpeOpt ~ Some(ass) => VarDefinitionWithAssignment(name, tpeOpt, ass)
  }

  private val echo = "echo" ~> "(" ~> expr <~ ")" ^^ ASTNode.Echo

  private def block: Parser[Block] = "{" ~> rep(statement) <~ "}" ^^ Block

  private def ifSt = "if" ~> "(" ~> expr ~ ")" ~ block ~ opt("else" ~> block) ^^ {
    case cond ~ _ ~ thenBlock ~ elseBlockOpt =>
      IfBlock(cond, thenBlock.exprs, elseBlockOpt.map(_.exprs).getOrElse(Seq()))
  }

  private def statement: Parser[Expression] = echo | varDefinition | assignment | expr

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

  private val structField = varName ~ ":" ~ typeName ^^ {
    case varName ~ _ ~ typeName => StructDefinition.Field(varName, typeName)
  }

  private val structDefinition = "struct" ~> literal ~ ("(" ~> repsep(structField, ",") <~ ")") ^^ {
    case typeName ~ fields =>
      StructDefinition(typeName, fields)
  }

  private val parser: Parser[List[ASTNode]] = rep(structDefinition | fnDefinition | statement)

  def parse(r: java.io.Reader) = parseAll(parser, r)
}
