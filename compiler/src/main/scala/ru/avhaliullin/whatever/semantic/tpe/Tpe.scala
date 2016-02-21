package ru.avhaliullin.whatever.semantic.tpe

import ru.avhaliullin.whatever.frontend.syntax.{TypeExpression, Expression}
import ru.avhaliullin.whatever.semantic.ImportsContext
import ru.avhaliullin.whatever.semantic.module.ModuleName

/**
  * @author avhaliullin
  */
sealed trait Tpe {
  def name: String
}

object Tpe {

  sealed trait Predefined extends Tpe

  case object INT extends Predefined {
    val name = "Int"
  }

  case object BOOL extends Predefined {
    val name = "Boolean"
  }

  case object UNIT extends Predefined {
    val name = "Unit"
  }

  case object STRING extends Predefined {
    val name = "String"
  }

  case class UDT(name: String, module: ModuleName) extends Tpe

  case class Arr(elem: Tpe) extends Predefined {
    def name = "Array[" + elem.name + "]"
  }

  case object ANY extends Tpe {
    val name = "Any"
  }

  def getTpe(tpeExpr: TypeExpression, ic: ImportsContext): Tpe = {
    val (name, module) = ic.resolveName(tpeExpr.name)
    val res = if (module == ModuleName.Default.std) {
      name match {
        case "Int" => INT
        case "Boolean" => BOOL
        case "Unit" => UNIT
        case "String" => STRING
        case "Array" =>
          if (tpeExpr.args.size != 1) {
            throw new RuntimeException("Array type constructor takes one type parameter, found " + tpeExpr.args.size)
          }
          Arr(getTpe(tpeExpr.args.head, ic))
        case _ => throw new RuntimeException("Unknown type: " + tpeExpr.name)
      }
    } else {
      UDT(name, module)
    }
    if (tpeExpr.args.nonEmpty && !res.isInstanceOf[Arr]) {
      throw new RuntimeException(s"Type ${tpeExpr.name} doesn't take type parameters")
    }
    res
  }

}
