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

  sealed trait Statement extends ASTNode

  sealed trait Expression extends Statement

  case class Const(value: Int) extends Expression

  case class Variable(name: String) extends Expression

  case class Operator(l: Expression, r: Expression, op: String) extends Expression {
    override def toString: String = op + "(" + l + ", " + r + ")"
  }

  case class Echo(e: Expression) extends Statement

  case class Block(sts: List[Statement]) extends Statement

  case class Assignment(variable: String, expr: Expression) extends Statement

  case class VarDefinition(name: String) extends Statement

  case class FnCall(name: String, args: Seq[Expression]) extends Expression

  case class FnDefinition(signature: FnDefinition.Signature, code: List[Statement]) extends ASTNode with Definition

  object FnDefinition {

    case class Signature(name: String, returnT: String, args: Seq[FnDefinition.Arg])

    case class Arg(name: String, tpe: String)

  }

}
