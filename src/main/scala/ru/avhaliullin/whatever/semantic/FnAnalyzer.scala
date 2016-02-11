package ru.avhaliullin.whatever.semantic

import ru.avhaliullin.whatever.syntax.{SyntaxTreeNode => syn}

/**
  * @author avhaliullin
  */
class FnAnalyzer(ts: TypesStore) {
  def convertSignature(sig: syn.FnDefinition.Signature): FnSignature = {
    FnSignature(
      sig.name,
      sig.args.map(arg => FnSignature.Arg(arg.name, ts.getPassable(arg.tpe))),
      ts.getAny(sig.returnT)
    )
  }

  def generateFnSigs(raw: Seq[syn.Definition]): (Map[String, Set[FnSignature]], Map[syn.FnDefinition, FnSignature]) = {
    val name2Fns = raw.collect {
      case rawDef@syn.FnDefinition(sig, _) => convertSignature(sig) -> rawDef
    }.groupBy(_._1.name)

    val name2FnsSet = name2Fns.mapValues {
      overloaded =>
        overloaded.foldLeft(Set[FnSignature]()) {
          case (acc, (it, _)) =>
            if (acc(it)) {
              throw new RuntimeException(s"Duplicated method with signature $it")
            } else {
              acc + it
            }
        }
    }

    val rawDef2TypedDef = name2Fns.toSeq.flatMap(_._2).map(_.swap).toMap
    (name2FnsSet, rawDef2TypedDef)
  }

}
