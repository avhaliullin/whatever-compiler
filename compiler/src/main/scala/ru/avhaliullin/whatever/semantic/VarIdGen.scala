package ru.avhaliullin.whatever.semantic

/**
  * @author avhaliullin
  */
class VarIdGen {
  private var id = 0

  def nextVar(name: String) = {
    id += 1
    VarId.LocalVar(name, id)
  }

  def methodArg(name: String) = VarId.MethodArg(name)
}
