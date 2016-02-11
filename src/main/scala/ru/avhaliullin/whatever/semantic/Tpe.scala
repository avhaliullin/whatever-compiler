package ru.avhaliullin.whatever.semantic

/**
  * @author avhaliullin
  */
sealed trait Tpe {
  def name: String
}

object Tpe {

  sealed trait Predefined extends Tpe

  sealed trait Passable extends Tpe

  case object INT extends Predefined with Passable {
    val name = "Int"
  }

  case object BOOL extends Predefined with Passable {
    val name = "Boolean"
  }

  case object UNIT extends Predefined {
    val name = "Unit"
  }

  case object STRING extends Predefined with Passable {
    val name = "String"
  }

  case object ARGS extends Predefined with Passable {
    val name = "String[]"
  }

  case class Struct(name: String) extends Tpe with Passable

  def getPrimitiveOpt(name: String): Option[Tpe] = {
    name match {
      case "Int" => Some(INT)
      case "Boolean" => Some(BOOL)
      case "Unit" => Some(UNIT)
      case "String" => Some(STRING)
      case _ => None
    }
  }

}
