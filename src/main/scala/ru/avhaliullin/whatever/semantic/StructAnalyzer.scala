package ru.avhaliullin.whatever.semantic

import ru.avhaliullin.whatever.semantic.tpe.{UserDefinedTypes, Tpe}
import ru.avhaliullin.whatever.syntax.{SyntaxTreeNode => syn}

/**
  * @author avhaliullin
  */
class StructAnalyzer {
  def convertStructures(nodes: Seq[syn.StructDefinition]): Map[String, Structure] = {
    val name2Node = nodes.map(sd => sd.name -> sd).toMap
    val udts = UserDefinedTypes(name2Node.keys)
    name2Node.mapValues {
      sd =>
        Structure(
          sd.name,
          sd.fields.map {
            field =>
              val fieldTpe = Tpe.getTpe(field.tpe, udts)
              Structure.Field(field.name, fieldTpe)
          }.toIndexedSeq
        )
    }
  }
}
