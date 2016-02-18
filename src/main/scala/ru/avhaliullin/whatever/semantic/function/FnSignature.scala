package ru.avhaliullin.whatever.semantic.function

import ru.avhaliullin.whatever.semantic.module.ModuleName
import ru.avhaliullin.whatever.semantic.tpe.Tpe

/**
  * @author avhaliullin
  */
case class FnSignature(name: String, args: Seq[FnSignature.Arg], returnType: Tpe, module: ModuleName)

object FnSignature {

  case class Arg(name: String, tpe: Tpe)

}
