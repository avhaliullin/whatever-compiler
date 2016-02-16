package ru.avhaliullin.whatever.semantic

import ru.avhaliullin.whatever.common.PrettyPrint
import ru.avhaliullin.whatever.semantic.function.FnSignature
import ru.avhaliullin.whatever.semantic.tpe.Tpe

/**
  * @author avhaliullin
  */
sealed trait SemanticTreeNode {
  def pretty: PrettyPrint
}

object SemanticTreeNode {

  sealed trait Definition extends SemanticTreeNode

  sealed trait Expression extends SemanticTreeNode {
    def tpe: Tpe

    def valRet: Boolean = true

    def mute: Expression

    def pretty = prettyExpr.prepend(s"[$tpe]")

    def prettyExpr: PrettyPrint
  }

  sealed trait Statement extends Expression {
    override final def tpe: Tpe = Tpe.UNIT

    def mute = this

    override val valRet = false
  }

  case class VarDefinition(id: VarId, varType: Tpe) extends Statement {
    def prettyExpr = PrettyPrint.Literal(s"VarDef($id : $varType)")
  }

  sealed trait Assignment extends Expression {
    def value: Expression

    def read: Boolean

    override def valRet = read

    def tpe = if (read) value.tpe else Tpe.UNIT
  }

  case class VarAssignment(id: VarId, value: Expression, read: Boolean) extends Assignment {
    def mute = if (read) copy(read = false) else this

    def prettyExpr = PrettyPrint.Complex(s"Assign(", ")", Seq(PrettyPrint.Literal(id.toString), value.pretty))
  }

  case class FieldAccess(field: Structure.Field, structure: Structure, expr: Expression) extends Expression {
    def tpe = field.tpe

    def mute = expr.mute

    def prettyExpr = PrettyPrint.Complex(s"FieldAccess(", ")", Seq(PrettyPrint.Literal(structure.name + "." + field.name), expr.pretty))
  }

  case class FieldAssignment(field: FieldAccess, value: Expression, read: Boolean) extends Assignment {
    def mute = if (read) copy(read = false) else this

    def prettyExpr = PrettyPrint.Complex(s"Assignment(", ")", Seq(field.pretty, value.pretty))
  }

  case class VarRead(id: VarId, tpe: Tpe) extends Expression {
    def mute = Nop

    def prettyExpr = PrettyPrint.Literal(s"VarRead($id)")
  }

  case class Block(code: Seq[SemanticTreeNode.Expression], tpe: Tpe) extends Expression {
    def mute: Block = {
      if (tpe == Tpe.UNIT) {
        this
      } else {
        Block(code.dropRight(1) :+ code.last.mute, Tpe.UNIT)
      }
    }

    override def valRet = if (code.isEmpty) false else code.last.valRet

    def prettyExpr = PrettyPrint.Complex("{", "}", code.map(_.pretty))
  }

  case class StructureInstantiation(desc: Structure, args: IndexedSeq[Expression], evalOrder: Seq[Int]) extends Expression {
    val tpe = Tpe.Struct(desc.name)

    def mute = {
      Block(evalOrder.map(args(_).mute), Tpe.UNIT)
    }

    def prettyExpr = PrettyPrint.Complex(s"new ${desc.name}(", ")", args.map(_.pretty) :+ PrettyPrint.Literal("evaluation order: " + evalOrder.mkString(", ")))
  }

  case class StructureDefinition(desc: Structure) extends Definition {
    def pretty = PrettyPrint.Literal("struct " + desc.name + "(" + desc.fields.map(f => f.name + ": " + f.tpe).mkString(", ") + ")")
  }

  case class FnDefinition(sig: FnSignature, code: Seq[SemanticTreeNode.Expression]) extends Definition {
    def pretty = PrettyPrint.Complex("fn " + sig.name + "(" + sig.args.map(a => a.name + ": " + a.tpe).mkString(", ") + "): " + sig.returnType + " = {", "}", code.map(_.pretty))
  }

  case class FnCall(sig: FnSignature, args: Seq[Expression]) extends Expression {
    override def tpe: Tpe = sig.returnType

    def mute = {
      if (!valRet) {
        this
      } else {
        Consume(this)
      }
    }

    override def valRet = tpe != Tpe.UNIT

    def prettyExpr = PrettyPrint.Complex("FnCall(", ")", PrettyPrint.Literal(sig.name) +: args.map(_.pretty))
  }

  case class Echo(expr: Expression) extends Statement {
    def prettyExpr = PrettyPrint.Complex("Echo(", ")", Seq(expr.pretty))
  }

  case class BOperator(arg1: Expression, arg2: Expression, op: BinaryOperator) extends Expression {
    val tpe = op.retType

    def mute = {
      if (op.conditionalEval) {
        Consume(this)
      } else {
        Block(Seq(arg1.mute, arg2.mute), Tpe.UNIT)
      }
    }

    def prettyExpr = PrettyPrint.Complex("BinOp-" + op + "(", ")", Seq(arg1.pretty, arg2.pretty))
  }

  case class UOperator(arg: Expression, op: UnaryOperator) extends Expression {
    val tpe = op.retType

    def mute = arg.mute

    def prettyExpr = PrettyPrint.Complex("UnOp-" + op + "(", ")", Seq(arg.pretty))
  }

  sealed trait Const extends Expression {
    def literal: String

    def mute = Nop

    def prettyExpr = PrettyPrint.Literal(literal)
  }

  case class IntConst(value: Int) extends Const {
    val tpe = Tpe.INT

    def literal = value.toString
  }

  case class BoolConst(value: Boolean) extends Const {
    val tpe = Tpe.BOOL

    def literal = value.toString
  }

  case class StringConst(value: String) extends Const {
    val tpe = Tpe.STRING

    def literal = value
  }

  case class IfExpr(cond: Expression, thenBlock: Expression, elseBlock: Expression, tpe: Tpe) extends Expression {
    def mute = {
      if (tpe == Tpe.UNIT) {
        this
      } else {
        IfExpr(cond, thenBlock.mute, elseBlock.mute, Tpe.UNIT)
      }
    }

    def prettyExpr = PrettyPrint.Complex("If(", ")", Seq(cond.pretty.prepend("condition: "), thenBlock.pretty.prepend("then: "), elseBlock.pretty.prepend("else: ")))
  }

  case object Nop extends Statement {
    def prettyExpr = PrettyPrint.Literal("NOP")
  }

  case class Consume(expr: Expression) extends Statement {
    def prettyExpr = PrettyPrint.Complex(s"Consume(", ")", Seq(expr.pretty))
  }

  case class ArrayInstantiation(elemTpe: Tpe, args: Seq[Expression]) extends Expression {
    val tpe = Tpe.Arr(elemTpe)

    override def mute: Expression = Block(args.map(_.mute), tpe)

    override def prettyExpr: PrettyPrint = PrettyPrint.Complex("Arr(", ")", args.map(_.pretty))
  }

  case class ArrayLength(array: Expression) extends Expression {
    val tpe = Tpe.INT

    override def mute: Expression = Nop

    override def prettyExpr: PrettyPrint = PrettyPrint.Complex("ArrLength(", ")", Seq(array.pretty))
  }

  case class ArrayGet(array: Expression, index: Expression, tpe: Tpe) extends Expression {
    override def mute: Expression = Block(Seq(array.mute, index.mute), Tpe.UNIT)

    override def prettyExpr: PrettyPrint = PrettyPrint.Complex(
      "ArrGet(",
      ")",
      Seq(array.pretty.prepend("array: "), index.pretty.prepend("index: "))
    )
  }

  case class ArraySet(array: Expression, index: Expression, value: Expression) extends Statement {
    override def prettyExpr: PrettyPrint = PrettyPrint.Complex(
      "ArrSet(",
      ")",
      Seq(array.pretty.prepend("array: "), index.pretty.prepend("index: "), value.pretty.prepend("value: "))
    )
  }

  case class ForLoop(itVar: VarId, iterable: Expression, body: Seq[Expression]) extends Statement {
    override def prettyExpr: PrettyPrint = PrettyPrint.Complex(
      "For(" + itVar,
      ")",
      iterable.pretty.prepend("in: ") +: body.map(_.pretty)
    )
  }

}
