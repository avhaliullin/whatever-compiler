package ru.avhaliullin.exp.typed

/**
  * @author avhaliullin
  */
sealed trait Tpe

object Tpe {

  sealed trait Predefined extends Tpe

  case object INT extends Predefined

  case object BOOL extends Predefined

  case object UNIT extends Predefined

  def apply(name: String): Tpe = {
    name match {
      case "int" => INT
      case "boolean" => BOOL
      case "unit" => UNIT
      case _ => throw new RuntimeException(s"Unknown type $name")
    }
  }

}
