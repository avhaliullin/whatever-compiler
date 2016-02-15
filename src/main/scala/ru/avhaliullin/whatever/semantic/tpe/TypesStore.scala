package ru.avhaliullin.whatever.semantic.tpe

import ru.avhaliullin.whatever.semantic.Structure
import ru.avhaliullin.whatever.syntax.SyntaxTreeNode

/**
  * @author avhaliullin
  */
trait UserDefinedTypes {
  def hasStruct(name: String): Boolean
}

object UserDefinedTypes {
  def apply(structNames: Iterable[String]) = new UserDefinedTypes {
    private val names = structNames.toSet

    override def hasStruct(name: String): Boolean = names(name)
  }
}

class TypesStore(structs: Map[String, Structure]) extends UserDefinedTypes {

  def getAny(name: SyntaxTreeNode.TypeExpression): Tpe = {
    Tpe.getTpe(name, this)
  }

  def getStruct(name: String): Structure = structs(name)

  def hasStruct(name: String) = structs.contains(name)
}

