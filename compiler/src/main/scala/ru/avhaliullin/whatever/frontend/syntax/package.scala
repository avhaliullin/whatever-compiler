package ru.avhaliullin.whatever.frontend

/**
  * @author avhaliullin
  */
package object syntax {

  case class SyntaxTree(nodes: Seq[Definition], imports: Seq[Import])

  case class TypeExpression(name: QualifiedName, args: Seq[TypeExpression])

  case class Import(name: QualifiedName)

  type FnDefinition = Definition.FnDefinition
  val FnDefinition = Definition.FnDefinition

  type StructDefinition = Definition.StructDefinition
  val StructDefinition = Definition.StructDefinition

  type StructImplementation = Definition.StructImplementation
  val StructImplementation = Definition.StructImplementation
}
