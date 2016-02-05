package ru.avhaliullin.exp.typed

/**
  * @author avhaliullin
  */
case class Structure(name: String, fields: IndexedSeq[Structure.Field])

object Structure {

  case class Field(name: String, tpe: Tpe)

}
