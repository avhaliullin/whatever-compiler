package ru.avhaliullin.exp.typed

import ru.avhaliullin.exp.typed.TypedASTNode.Expression

/**
  * @author avhaliullin
  */
object Operator {
  private val binOps: Map[(Tpe, Tpe, String), (Expression, Expression) => TypedASTNode.BinaryOperator] =
    Map(
      (Tpe.INT, Tpe.INT, "+") -> TypedASTNode.OpIAdd,
      (Tpe.INT, Tpe.INT, "-") -> TypedASTNode.OpISub,
      (Tpe.INT, Tpe.INT, "*") -> TypedASTNode.OpIMul,
      (Tpe.INT, Tpe.INT, "/") -> TypedASTNode.OpIDiv,

      (Tpe.INT, Tpe.INT, "<") -> TypedASTNode.OpILt,
      (Tpe.INT, Tpe.INT, "<=") -> TypedASTNode.OpILte,
      (Tpe.INT, Tpe.INT, ">") -> TypedASTNode.OpIGt,
      (Tpe.INT, Tpe.INT, ">=") -> TypedASTNode.OpIGte,
      (Tpe.INT, Tpe.INT, "==") -> TypedASTNode.OpIEq,
      (Tpe.INT, Tpe.INT, "!=") -> TypedASTNode.OpINeq
    )

  def apply(l: Expression, r: Expression, name: String): TypedASTNode.BinaryOperator = {
    binOps.getOrElse((l.tpe, r.tpe, name), throw new RuntimeException(s"Unknown operator ${l.tpe} $name ${r.tpe}"))(l, r)
  }

  private val unaryOps: Map[(Tpe, String), Expression => TypedASTNode.UnaryOperator] =
    Map(
      (Tpe.INT, "-") -> TypedASTNode.OpINeg
    )

  def apply(arg: Expression, name: String): TypedASTNode.UnaryOperator = {
    unaryOps.getOrElse((arg.tpe, name), throw new RuntimeException(s"Unknown operator ${arg.tpe} $name"))(arg)
  }
}
