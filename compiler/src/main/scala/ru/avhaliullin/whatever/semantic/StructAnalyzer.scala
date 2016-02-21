package ru.avhaliullin.whatever.semantic

import ru.avhaliullin.whatever.frontend.{syntax => syn}
import ru.avhaliullin.whatever.semantic.tpe.Tpe

/**
  * @author avhaliullin
  */
class StructAnalyzer {
  def convertStructure(sd: syn.StructDefinition, ic: ImportsContext): Structure = {
    Structure(
      Tpe.UDT(sd.name, ic.module),
      sd.fields.map {
        field =>
          val fieldTpe = Tpe.getTpe(field.tpe, ic)
          Structure.Field(field.name, fieldTpe)
      }.toIndexedSeq,
      Map()
    )
  }

}

object StructAnalyzer extends StructAnalyzer