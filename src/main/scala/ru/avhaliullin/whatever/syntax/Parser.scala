package ru.avhaliullin.whatever.syntax

import ru.avhaliullin.whatever.syntax.SyntaxTreeNode._

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * @author avhaliullin
  */
class Parser extends JavaTokenParsers {

  //  val literal: Parser[String] = """[a-zA-Z_][a-zA-Z_0-9]*""".r
  private val literal: Parser[String] = ident

  private val varName = literal

  private def typeExpression: Parser[TypeExpression] = literal ~ opt("[" ~> rep1sep(typeExpression, ",") <~ "]") ^^ {
    case name ~ argsOpt =>
      TypeExpression(name, argsOpt.getOrElse(Nil))
  }

  private val fnName = literal

  private val intConst = ("""-?[0-9]+""".r ^^ (_.toInt)) ^^ IntConst

  private val boolConst = ("true" ^^ (_ => true) | "false" ^^ (_ => false)) ^^ BoolConst

  private val stringConst = stringLiteral ^^ {
    case s => StringConst(s.substring(1, s.length - 1))
  }

  private val const = intConst | boolConst | stringConst

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
      case Some(name) ~ value => Argument.ByName(name, value)
      case None ~ value => Argument.ByOrder(value)
    }, ",") <~ ")"

  private def fArgsList = "(" ~> repsep(expr, ",") <~ ")"

  private def fCall = fnName ~ fArgsList ^^ {
    case name ~ args =>
      FnCall(name, args)
  }

  private def sInstantiation = literal ~ argList ^^ {
    case tName ~ args =>
      StructInstantiation(tName, args)
  }


  private def arrInstantiation = ("Array" ~> ("[" ~> typeExpression <~ "]").?) ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case tpeOpt ~ exprs => ArrayInstantiation(tpeOpt, exprs)
  }

  private def instantiation = "new" ~> (arrInstantiation | sInstantiation)

  private def term = instantiation | ifSt | fCall | value | par | block

  private sealed trait Member

  private case class Field(name: String) extends Member

  private case class Method(name: String, args: Seq[Expression]) extends Member

  private def member = term ~ rep("." ~> (fnName ~ fArgsList ^^ {
    case mName ~ exprs => Method(mName, exprs)
  } | varName ^^ Field)) ^^ {
    case e0 ~ members => members.foldLeft(e0) {
      case (acc, Field(fieldName)) =>
        FieldAccess(fieldName, acc)
      case (acc, Method(methodName, args)) =>
        MethodCall(acc, methodName, args)
    }
  }

  // unary
  private val unaryRegex =
    """-|!""".r

  private def unary = rep(unaryRegex) ~ member ^^ {
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
          Assignment(assignee, value)
      }
  }

  private def binary = assignment


  private def expr: Parser[Expression] = binary


  private val varDefinition = "var" ~> varName ~ (":" ~> typeExpression).? ~ ("=" ~> expr).? ^^ {
    case name ~ None ~ None => throw new RuntimeException(s"Variable $name doesn't have type nor assigned")
    case name ~ Some(tpe) ~ None => VarDefinition(name, tpe)
    case name ~ tpeOpt ~ Some(ass) => VarDefinitionWithAssignment(name, tpeOpt, ass)
  }

  private val echo = "echo" ~> "(" ~> expr <~ ")" ^^ Echo

  private def block: Parser[Block] = "{" ~> rep(statement) <~ "}" ^^ Block

  private def ifSt = "if" ~> "(" ~> expr ~ ")" ~ block ~ opt("else" ~> block) ^^ {
    case cond ~ _ ~ thenBlock ~ elseBlockOpt =>
      IfBlock(cond, thenBlock.exprs, elseBlockOpt.map(_.exprs).getOrElse(Seq()))
  }

  private def statement: Parser[Expression] = forLoop | echo | varDefinition | assignment | expr

  private val arg = varName ~ ":" ~ typeExpression ^^ {
    case varName ~ _ ~ typeName => FnDefinition.Arg(varName, typeName)
  }

  private val fnSignature = "fn" ~> fnName ~ "(" ~ repsep(arg, ",") ~ ")" ~ ":" ~ typeExpression ^^ {
    case fnName ~ _ ~ args ~ _ ~ _ ~ retType =>
      FnDefinition.Signature(fnName, retType, args)
  }

  private val fnDefinition = fnSignature ~ "=" ~ "{" ~ rep(statement) <~ "}" ^^ {
    case sig ~ _ ~ _ ~ code =>
      FnDefinition(sig, code)
  }

  private val structField = varName ~ ":" ~ typeExpression ^^ {
    case varName ~ _ ~ typeName => StructDefinition.Field(varName, typeName)
  }

  private val structDefinition = "struct" ~> literal ~ ("(" ~> repsep(structField, ",") <~ ")") ^^ {
    case typeName ~ fields =>
      StructDefinition(typeName, fields)
  }

  private def forLoop = ("for" ~> "(" ~> varName ~ "<-" ~ expr <~ ")") ~ block ^^ {
    case it ~ _ ~ src ~ body => ForLoop(it, src, body)
  }

  private val parser: Parser[List[SyntaxTreeNode]] = rep(structDefinition | fnDefinition | statement)

  def parse(r: java.io.Reader) = parseAll(parser, r)
}
