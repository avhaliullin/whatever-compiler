package ru.avhaliullin.exp.ast

/**
  * @author avhaliullin
  */
sealed trait ASTNode {
  def isDefinition: Boolean = false
}

object ASTNode {

  sealed trait Definition extends ASTNode {
    override def isDefinition = true
  }


  sealed trait Expression extends ASTNode

  sealed trait Const extends Expression

  case class IntConst(value: Int) extends Const

  case class BoolConst(value: Boolean) extends Const

  case class Variable(name: String) extends Expression

  case class BinaryOperator(l: Expression, r: Expression, op: String) extends Expression {
    override def toString: String = op + "(" + l + ", " + r + ")"
  }

  case class UnaryOperator(arg: Expression, op: String) extends Expression

  case class Echo(e: Expression) extends Expression

  case class Block(exprs: List[Expression]) extends Expression

  case class Assignment(assignee: Expression, value: Expression) extends Expression

  case class FieldAccess(name: String, expr: Expression) extends Expression

  case class VarDefinition(name: String, tpe: String) extends Expression

  case class VarDefinitionWithAssignment(name: String, tpe: Option[String], expr: Expression) extends Expression

  case class IfBlock(cond: Expression, thenBlock: Seq[Expression], elseBlock: Seq[Expression]) extends Expression

  case class FnCall(name: String, args: Seq[Expression]) extends Expression

  case class FnDefinition(signature: FnDefinition.Signature, code: List[Expression]) extends ASTNode with Definition

  object FnDefinition {

    case class Signature(name: String, returnT: String, args: Seq[FnDefinition.Arg])

    case class Arg(name: String, tpe: String)

  }

  case class StructInstantiation(name: String, args: Seq[Argument]) extends Expression

  case class StructDefinition(name: String, fields: Seq[StructDefinition.Field]) extends ASTNode with Definition

  object StructDefinition {

    case class Field(name: String, tpe: String)

  }

  sealed trait Argument {
    def value: ASTNode.Expression
  }

  object Argument {

    case class ByName(name: String, value: ASTNode.Expression) extends Argument

    case class ByOrder(value: ASTNode.Expression) extends Argument

  }

}
