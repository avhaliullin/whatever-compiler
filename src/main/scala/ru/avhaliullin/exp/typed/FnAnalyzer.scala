package ru.avhaliullin.exp.typed

import ru.avhaliullin.exp.ast.ASTNode

/**
  * @author avhaliullin
  */
class FnAnalyzer(ts: TypesStore) {
  def convertSignature(sig: ASTNode.FnDefinition.Signature): FnSignature = {
    FnSignature(
      sig.name,
      sig.args.map(arg => FnSignature.Arg(arg.name, ts.getPassable(arg.tpe))),
      ts.getAny(sig.returnT)
    )
  }

  def generateFnSigs(raw: Seq[ASTNode.Definition]): (Map[String, Set[FnSignature]], Map[ASTNode.FnDefinition, FnSignature]) = {
    val name2Fns = raw.collect {
      case rawDef@ASTNode.FnDefinition(sig, _) => convertSignature(sig) -> rawDef
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