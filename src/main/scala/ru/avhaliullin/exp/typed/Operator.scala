package ru.avhaliullin.exp.typed

/**
  * @author avhaliullin
  */
sealed trait Operator {
  def literal: String

  def retType: Tpe
}

sealed trait BinaryOperator extends Operator {
  def arg1Type: Tpe

  def arg2Type: Tpe

  def conditionalEval: Boolean = false
}

sealed trait UnaryOperator extends Operator {
  def argType: Tpe
}

object Operator {

  protected sealed abstract class BinOp(val literal: String) extends BinaryOperator

  object BinarySelector {

    sealed trait X_X_Int extends BinaryOperator {
      override def retType = Tpe.INT
    }

    sealed trait Int_Int_X extends BinaryOperator {
      override def arg1Type = Tpe.INT

      override def arg2Type = Tpe.INT
    }

    sealed trait Int_Int_Int extends X_X_Int with Int_Int_X

    sealed trait X_X_Bool extends BinaryOperator {
      override def retType = Tpe.BOOL
    }

    sealed trait Bool_Bool_X extends BinaryOperator {
      override def arg1Type = Tpe.BOOL

      override def arg2Type = Tpe.BOOL
    }

    sealed trait Predicate extends X_X_Bool

    sealed trait Bool_Bool_Bool extends Bool_Bool_X with X_X_Bool

    sealed trait Int_Int_Bool extends Int_Int_X with Predicate

    sealed trait ConditionalEval extends BinaryOperator{
      override def conditionalEval: Boolean = true
    }
  }

  import BinarySelector._

  case object IADD extends BinOp("+") with Int_Int_Int

  case object ISUB extends BinOp("-") with Int_Int_Int

  case object IMUL extends BinOp("*") with Int_Int_Int

  case object IDIV extends BinOp("/") with Int_Int_Int


  case object ILT extends BinOp("<") with Int_Int_Bool

  case object ILE extends BinOp("<=") with Int_Int_Bool

  case object IGT extends BinOp(">") with Int_Int_Bool

  case object IGE extends BinOp(">=") with Int_Int_Bool

  case object IEQ extends BinOp("==") with Int_Int_Bool

  case object INE extends BinOp("!=") with Int_Int_Bool

  private val binOps: Map[(Tpe, Tpe, String), BinaryOperator] =
    Seq(
      IADD,
      ISUB,
      IMUL,
      IDIV,

      ILT,
      ILE,
      IGT,
      IGE,
      IEQ,
      INE
    ).map(op => (op.arg1Type, op.arg2Type, op.literal) -> op).toMap

  def apply(lTpe: Tpe, rTpe: Tpe, name: String): BinaryOperator = {
    binOps.getOrElse((lTpe, rTpe, name), throw new RuntimeException(s"Unknown operator $lTpe $name $rTpe"))
  }

  protected sealed abstract class UnOp(val literal: String, tpe: Tpe) extends UnaryOperator {
    val retType = tpe
    val argType = tpe
  }

  case object INEG extends UnOp("-", Tpe.INT)

  private val unaryOps: Map[(Tpe, String), UnaryOperator] =
    Seq(
      INEG
    ).map(op => (op.argType, op.literal) -> op).toMap

  def apply(argTpe: Tpe, name: String): UnaryOperator = {
    unaryOps.getOrElse((argTpe, name), throw new RuntimeException(s"Unknown operator $argTpe $name"))
  }
}
