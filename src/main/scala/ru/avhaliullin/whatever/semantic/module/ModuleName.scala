package ru.avhaliullin.whatever.semantic.module

/**
  * @author avhaliullin
  */
sealed trait ModuleName {
  def name: String

  def fqn: String

  def sub(name: String) = ModuleName.Nested(name, this)
}

object ModuleName {

  case class Nested(name: String, parent: ModuleName) extends ModuleName {
    val fqn = parent.fqn + "::" + name

    override def toString(): String = fqn

  }

  object ROOT extends ModuleName {
    val fqn = ""

    def name = ""

    override def toString() = "ROOT"
  }

  object Default {
    val std = ROOT.sub("std")
  }

}
