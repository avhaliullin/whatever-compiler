package ru.avhaliullin.exp.typed

import ru.avhaliullin.exp.ast.ASTNode

/**
  * @author avhaliullin
  */
class StructAnalyzer {
  def convertStructures(nodes: Seq[ASTNode.StructDefinition]): Map[String, Structure] = {
    val name2Node = nodes.map(sd => sd.name -> sd).toMap
    name2Node.mapValues {
      sd =>
        Structure(
          sd.name,
          sd.fields.map {
            field =>
              val fieldTpe = Tpe.getOpt(field.tpe) match {
                case Some(tpe) => tpe
                case None =>
                  if (!name2Node.contains(field.tpe)) {
                    throw new RuntimeException(s"Struct ${sd.name}: field ${field.name} have unknown type ${field.tpe}")
                  }
                  Tpe.Struct(field.tpe)
              }
              Structure.Field(field.name, fieldTpe)
          }
        )
    }
  }
}
