package ru.avhaliullin.exp.typed

/**
  * @author avhaliullin
  */
sealed trait VarId {
  def name: String
}

object VarId {

  case class MethodArg(name: String) extends VarId

  case class LocalVar(name: String, id: Int) extends VarId

  case class CompilerProduced(id: Int) extends VarId {
    val name = "$x" + id
  }

}
