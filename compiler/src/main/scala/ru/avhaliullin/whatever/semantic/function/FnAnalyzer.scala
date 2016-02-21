package ru.avhaliullin.whatever.semantic.function

import ru.avhaliullin.whatever.frontend.{syntax => syn}
import ru.avhaliullin.whatever.semantic.ImportsContext
import ru.avhaliullin.whatever.semantic.tpe.Tpe

/**
  * @author avhaliullin
  */
class FnAnalyzer {
  def convertSignature(sig: syn.FnDefinition.Signature, ic: ImportsContext): FnSignature = {
    FnSignature(
      sig.name,
      sig.args.map(arg => FnSignature.Arg(arg.name, Tpe.getTpe(arg.tpe, ic))),
      Tpe.getTpe(sig.returnT, ic),
      ic.module
    )
  }

}

object FnAnalyzer extends FnAnalyzer