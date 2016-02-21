package ru.avhaliullin.whatever.frontend.syntax

/**
  * @author avhaliullin
  */
sealed trait Definition

object Definition {

  case class FnDefinition(signature: FnDefinition.Signature, code: List[Expression]) extends Definition

  case class StructImplementation(structType: TypeExpression, methods: Seq[FnDefinition]) extends Definition

  case class StructDefinition(name: String, fields: Seq[StructDefinition.Field]) extends Definition


  object FnDefinition {

    case class Signature(name: String, returnT: TypeExpression, args: Seq[FnDefinition.Arg])

    case class Arg(name: String, tpe: TypeExpression)

  }

  object StructDefinition {

    case class Field(name: String, tpe: TypeExpression)

  }
}
