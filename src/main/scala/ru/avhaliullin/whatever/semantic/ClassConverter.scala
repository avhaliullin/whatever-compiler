package ru.avhaliullin.whatever.semantic

import ru.avhaliullin.whatever.syntax.{SyntaxTreeNode => syn}
import ru.avhaliullin.whatever.semantic.{SemanticTreeNode => sem}

/**
  * @author avhaliullin
  */
class ClassConverter {

  def convert(ast: Seq[syn.Definition]): (Seq[sem.FnDefinition], Seq[Structure]) = {
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

    val fnStore = new FnStore(fnName2Fns)

    val fc = new FnConverter(ts, fnStore, new VarIdGen)

    (fns.map {
      fn =>
        fc.convert(fn.code, fnRawSig2Typed(fn))
    }, structName2Struct.values.toSeq)
  }
}
