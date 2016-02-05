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

    def mute: Expression
  }

  sealed trait Statement extends Expression {
    override final def tpe: Tpe = Tpe.UNIT

    def mute = this
  }

  case class VarDefinition(id: VarId, varType: Tpe) extends Statement

  case class VarAssignment(id: VarId, expr: Expression) extends Statement

  case class VarRead(id: VarId, tpe: Tpe) extends Expression {
    def mute = Nop
  }

  case class Block(code: Seq[TypedASTNode.Expression], tpe: Tpe) extends Expression {
    def mute = {
      if (tpe == Tpe.UNIT) {
        this
      } else {
        Block(code.dropRight(1) :+ code.last.mute, Tpe.UNIT)
      }
    }
  }

  case class StructureInstantiation(desc: Structure, args: Seq[Expression], evalOrder: Seq[Int]) extends Expression {
    val tpe = Tpe.Struct(desc.name)

    def mute = {
      Block(evalOrder.map(args(_).mute), Tpe.UNIT)
    }
  }

  case class StructureDefinition(desc: Structure) extends Definition

  case class FnDefinition(sig: FnSignature, code: Seq[TypedASTNode.Expression]) extends Definition

//  case class Main(code: Seq[TypedASTNode.Expression]) extends Definition

  case class FnCall(sig: FnSignature, args: Seq[Expression]) extends Expression {
    override def tpe: Tpe = sig.returnType

    def mute = {
      if (tpe == Tpe.UNIT) {
        this
      } else {
        Block(Seq(this, Pop(tpe)), Tpe.UNIT)
      }
    }
  }

  case class Echo(expr: Expression) extends Statement

  case class BOperator(arg1: Expression, arg2: Expression, op: BinaryOperator) extends Expression {
    val tpe = op.retType

    def mute = {
      if (op.conditionalEval) {
        Block(Seq(this, Pop(op.retType)), Tpe.UNIT)
      } else {
        Block(Seq(arg1.mute, arg2.mute), Tpe.UNIT)
      }
    }
  }

  case class UOperator(arg: Expression, op: UnaryOperator) extends Expression {
    val tpe = op.retType

    def mute = arg.mute
  }

  sealed trait Const extends Expression {
    def mute = Nop
  }

  case class IntConst(value: Int) extends Const {
    val tpe = Tpe.INT
  }

  case class BoolConst(value: Boolean) extends Const {
    val tpe = Tpe.BOOL
  }

  case class IfExpr(cond: Expression, thenBlock: Expression, elseBlock: Expression, tpe: Tpe) extends Expression {
    def mute = {
      if (tpe == Tpe.UNIT) {
        this
      } else {
        IfExpr(cond, thenBlock.mute, elseBlock.mute, Tpe.UNIT)
      }
    }
  }

  case object Nop extends Statement

  case class Pop(argType: Tpe) extends Statement

}
