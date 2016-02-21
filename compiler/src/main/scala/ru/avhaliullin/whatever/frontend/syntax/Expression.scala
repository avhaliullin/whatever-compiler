package ru.avhaliullin.whatever.frontend.syntax

/**
  * @author avhaliullin
  */
sealed trait Expression

object Expression {

  sealed trait Const extends Expression

  case class IntConst(value: Int) extends Const

  case class BoolConst(value: Boolean) extends Const

  case class StringConst(value: String) extends Const

  case class Variable(name: String) extends Expression

  case class BinaryOperator(l: Expression, r: Expression, op: String) extends Expression {
    override def toString: String = op + "(" + l + ", " + r + ")"
  }

  case class UnaryOperator(arg: Expression, op: String) extends Expression

  case class Echo(e: Expression) extends Expression

  case class Block(exprs: List[Expression]) extends Expression

  case class Assignment(assignee: Expression, value: Expression) extends Expression

  case class FieldAccess(name: String, expr: Expression) extends Expression

  case class MethodCall(expr: Expression, name: String, args: Seq[Expression]) extends Expression

  case class VarDefinition(name: String, tpe: TypeExpression) extends Expression

  case class VarDefinitionWithAssignment(name: String, tpe: Option[TypeExpression], expr: Expression) extends Expression

  case class IfBlock(cond: Expression, thenBlock: Seq[Expression], elseBlock: Seq[Expression]) extends Expression

  case class FnCall(name: QualifiedName, args: Seq[Expression]) extends Expression

  case class StructInstantiation(name: QualifiedName, args: Seq[Argument]) extends Expression

  case class ArrayInstantiation(tpeOpt: Option[TypeExpression], elements: Seq[Expression]) extends Expression

  case class ForLoop(itVarName: String, iterable: Expression, body: Block) extends Expression

  sealed trait Argument {
    def value: Expression
  }

  object Argument {

    case class ByName(name: String, value: Expression) extends Argument

    case class ByOrder(value: Expression) extends Argument

  }

}
