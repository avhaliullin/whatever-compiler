package ru.avhaliullin.whatever.semantic

import ru.avhaliullin.whatever.semantic.function.FnSignature
import ru.avhaliullin.whatever.semantic.tpe.Tpe

/**
  * @author avhaliullin
  */
case class Structure(fullTpe: Tpe.UDT, fields: IndexedSeq[Structure.Field], methods: Map[String, FnSignature]){
  val fieldsMap = fields.map(f => f.name -> f).toMap
}

object Structure {

  case class Field(name: String, tpe: Tpe)

}
