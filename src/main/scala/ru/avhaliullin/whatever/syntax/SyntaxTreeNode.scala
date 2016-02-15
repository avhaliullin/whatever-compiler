package ru.avhaliullin.whatever.syntax

/**
  * @author avhaliullin
  */
sealed trait SyntaxTreeNode {
  def isDefinition: Boolean = false
}

object SyntaxTreeNode {

  sealed trait Definition extends SyntaxTreeNode {
    override def isDefinition = true
  }


  sealed trait Expression extends SyntaxTreeNode

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

  case class FnCall(name: String, args: Seq[Expression]) extends Expression

  case class FnDefinition(signature: FnDefinition.Signature, code: List[Expression]) extends SyntaxTreeNode with Definition

  object FnDefinition {

    case class Signature(name: String, returnT: TypeExpression, args: Seq[FnDefinition.Arg])

    case class Arg(name: String, tpe: TypeExpression)

  }

  case class StructInstantiation(name: String, args: Seq[Argument]) extends Expression

  case class StructDefinition(name: String, fields: Seq[StructDefinition.Field]) extends SyntaxTreeNode with Definition

  object StructDefinition {

    case class Field(name: String, tpe: TypeExpression)

  }

  sealed trait Argument {
    def value: SyntaxTreeNode.Expression
  }

  object Argument {

    case class ByName(name: String, value: SyntaxTreeNode.Expression) extends Argument

    case class ByOrder(value: SyntaxTreeNode.Expression) extends Argument

  }

  case class TypeExpression(name: String, args: Seq[TypeExpression]) extends SyntaxTreeNode

  case class ArrayInstantiation(tpeOpt: Option[TypeExpression], elements: Seq[Expression]) extends Expression

  case class ForLoop(itVarName: String, iterable: Expression, body: Block) extends Expression

}
