package ru.avhaliullin.whatever.semantic.module

import ru.avhaliullin.whatever.semantic.Structure
import ru.avhaliullin.whatever.semantic.function.FnSignature

/**
  * @author avhaliullin
  */
case class ModuleAPI(name: ModuleName, functions: Map[String, FnSignature], structs: Map[String, Structure]) {
  def withFunction(fn: FnSignature): ModuleAPI = {
    if (functions.contains(fn.name)) {
      throw new RuntimeException(s"Duplicated function ${fn.name} in module $name")
    }
    copy(functions = functions + (fn.name -> fn))
  }

  def withStruct(st: Structure): ModuleAPI = {
    if (structs.contains(st.fullTpe.name)) {
      throw new RuntimeException(s"Duplicated struct ${st.fullTpe.name} in module $name")
    }
    copy(structs = structs + (st.fullTpe.name -> st))
  }
}