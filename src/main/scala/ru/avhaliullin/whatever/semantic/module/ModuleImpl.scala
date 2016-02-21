package ru.avhaliullin.whatever.semantic.module

import ru.avhaliullin.whatever.semantic.Structure
import ru.avhaliullin.whatever.semantic.{SemanticTreeNode => sem}

/**
  * @author avhaliullin
  */
case class ModuleImpl(
                       name: ModuleName,
                       structs: Seq[Structure],
                       fns: Seq[sem.FnDefinition],
                       structImpls: Map[String, Seq[sem.FnDefinition]]
                     )
