package ru.avhaliullin.exp.typed

/**
  * @author avhaliullin
  */
class TypesStore(structs: Map[String, Structure]) {
  def getPassable(name: String): Tpe.Passable = {
    getAny(name) match {
      case p: Tpe.Passable => p
      case other => throw new RuntimeException(s"Type $other cannot be used here")
    }
  }

  def getAny(name: String): Tpe = {
    Tpe.getOpt(name).getOrElse(if (structs.contains(name)) Tpe.Struct(name) else throw new RuntimeException(s"Unknown type $name"))
  }

  def getStruct(name: String): Structure = structs(name)
}
