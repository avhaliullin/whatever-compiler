package ru.avhaliullin.whatever.semantic

import ru.avhaliullin.whatever.syntax.{SyntaxTreeNode => syn}
import ru.avhaliullin.whatever.semantic.{SemanticTreeNode => sem}

/**
  * @author avhaliullin
  */
class ClassConverter {
  private val mainSig = FnSignature("main", Seq(FnSignature.Arg("args", Tpe.ARGS)), Tpe.UNIT)

  def convert(ast: Seq[syn]): (Seq[sem.FnDefinition], Seq[Structure]) = {
    val structs = ast.collect {
      case st: syn.StructDefinition => st
    }

    val fns = ast.collect {
      case fn: syn.FnDefinition => fn
    }

    val sa = new StructAnalyzer
    val structName2Struct = sa.convertStructures(structs)

    val ts = new TypesStore(structName2Struct)
    val fa = new FnAnalyzer(ts)
    val (fnName2Fns, fnRawSig2Typed) = fa.generateFnSigs(fns)

    val exprs = ast.collect {
      case expr: syn.Expression => expr
    }

    val fnStore = new FnStore(fnName2Fns)

    val fc = new FnConverter(ts, fnStore, new VarIdGen)

    val mainConverted = fc.convert(exprs, mainSig)

    (fns.map {
      fn =>
        fc.convert(fn.code, fnRawSig2Typed(fn))
    } :+ mainConverted, structName2Struct.values.toSeq)
  }
}
