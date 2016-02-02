package ru.avhaliullin.exp.typed

/**
  * @author avhaliullin
  */
sealed trait TypedASTNode {

}

object TypedASTNode {

  sealed trait Definition extends TypedASTNode

  sealed trait Expression extends TypedASTNode {
    def tpe: Tpe
  }

  sealed trait Statement extends Expression {
    override final def tpe: Tpe = Tpe.UNIT
  }

  case class VarDefinition(id: VarId, varType: Tpe) extends Statement

  case class VarAssignment(id: VarId, expr: Expression) extends Statement

  case class VarRead(id: VarId, tpe: Tpe) extends Expression

  case class Block(code: Seq[TypedASTNode.Expression], tpe: Tpe) extends Expression

  case class FnDefinition(sig: FnDefinition.Signature, code: Seq[TypedASTNode.Expression]) extends Definition

  case class Main(code: Seq[TypedASTNode.Expression]) extends Definition

  object FnDefinition {

    case class Signature(name: String, args: Seq[Arg], returnType: Tpe)

    case class Arg(name: String, tpe: Tpe)

  }

  case class FnCall(sig: FnDefinition.Signature, args: Seq[Expression]) extends Expression {
    override def tpe: Tpe = sig.returnType
  }

  case class Echo(expr: Expression) extends Statement

  sealed abstract class BinaryOperator(val tpe: Tpe) extends Expression {
    val arg1: Expression
    val arg2: Expression
  }

  case class OpIAdd(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.INT)
  case class OpISub(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.INT)
  case class OpIMul(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.INT)
  case class OpIDiv(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.INT)

  sealed trait ICmp extends BinaryOperator

  case class OpILt(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.BOOL) with ICmp
  case class OpILte(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.BOOL) with ICmp
  case class OpIGt(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.BOOL) with ICmp
  case class OpIGte(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.BOOL) with ICmp
  case class OpIEq(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.BOOL) with ICmp
  case class OpINeq(arg1: Expression, arg2: Expression) extends BinaryOperator(Tpe.BOOL) with ICmp

  sealed abstract class UnaryOperator(val tpe: Tpe) extends Expression {
    val arg: Expression
  }

  case class OpINeg(arg: Expression) extends UnaryOperator(Tpe.INT)

  sealed trait Const extends Expression

  case class IntConst(value: Int) extends Const {
    val tpe = Tpe.INT
  }

  case class BoolConst(value: Boolean) extends Const {
    val tpe = Tpe.BOOL
  }

}
